%%%----------------------------------------------------------------------
%%% File    : email.erl
%%% Author  : qingchuwudi <privmail@foxmail.com>
%%% Purpose : send email
%%% Changed : 2016-3-8 by qingchuwudi <privmail@foxmail.com>
%%%
%%%
%%% erlang_email, Copyright (C) 2016-2016   qingchuwudi
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------
-module(email_content).
-include("email.hrl").

-export([start/0, send/1]).
-compile(export_all).

-define(MAX_SIZE, 1024).

start() ->
    ok = ssl:start().

%% send email by email record
send(Email) when
        undefined =/= Email#email.server_ip,
        undefined =/= Email#email.account,
        undefined =/= Email#email.to_emails,
        undefined =/= Email#email.password ->
    ServerPort = case Email#email.server_port of
        undefined -> 
            case Email#email.ssl of
                true  -> ?SSL_SERV_PORT_DEF;
                false -> ?NOT_SSL_SERV_PORT_DEF
            end;
        Any -> Any
    end,
    Sock = case Email#email.ssl of
        false -> 
            {ok, Socket} = gen_tcp:connect(
                                    Email#email.server_ip,
                                    ServerPort,
                                    [binary, {active, false}, {packet, 0}]),
            #socket{type = tcp, sock = Socket};
        true  ->
            application:ensure_started(ssl),
            % ok = ssl:start(),
            {ok, Socket} = ssl:connect(
                            Email#email.server_ip,
                            ServerPort,
                            [binary, {active, false}, {packet, 0}],
                            infinity),
            #socket{type = ssl, sock = Socket}
    end,
    connect_email(Sock, Email),
    send_email_head(Sock, Email),
    send_email_info(Sock, Email),
    send_email_data(Sock, Email),
    end_email(Sock),
    case Sock#socket.type of
        ssl -> ssl:close(Sock#socket.sock);
        tcp -> gen_tcp:close(Sock#socket.sock)
    end.

%% 连接邮件服务器
connect_email(Sock, Email) ->
   send_socket(Sock, "HELO " ++ Email#email.account ++ "\r\n"),
   recv_socket(Sock),

   send_socket(Sock, "AUTH LOGIN\r\n"),
   recv_socket(Sock),

   send_socket(Sock, base64:encode(Email#email.account)),
   send_socket(Sock, "\r\n"),
   recv_socket(Sock),

   send_socket(Sock, base64:encode(Email#email.password)),
   send_socket(Sock, "\r\n"),
   recv_socket(Sock).

%% 邮件头，谁发送，发送给谁
send_email_head(Sock, Email) ->
    send_socket(Sock, "MAIL FROM <" ++ Email#email.account ++ ">\r\n"),
    recv_socket(Sock),

    rcpt_to_emails(Sock, Email#email.to_emails),
    recv_socket(Sock).

%% 邮件信息，主题等
send_email_info(Sock, Email) ->
    send_socket(Sock, "DATA\r\n"),
    recv_socket(Sock),

    send_socket(Sock, "FROM:<" ++ Email#email.account ++ ">\r\n"),
    recv_socket(Sock),
    Subject = unicode:characters_to_list(Email#email.subject),
    send_socket(Sock, "SUBJECT: "++ Subject ++ "\r\n").

%% 邮件内容
send_email_data(Sock, Email) when Email#email.content =/= undefined ->
    send_socket(Sock, "MIME-VERSION: 1.0\r\n"),
    send_socket(Sock, "Content-Type:text/html;charset=UTF-8\r\n\r\n"),
    send_email_content(Email#email.content, Sock);
send_email_data(_Sock, _Email) ->
    ok.

end_email(Sock) ->
    send_socket(Sock, "\r\n.\r\n"),
    recv_socket(Sock),
    send_socket(Sock, "QUIT\r\n"),
    recv_socket(Sock).

%% 直接发送内容
send_email_content(Data, Sock) ->
    ok = send(Sock, Data),
    send_socket(Sock, "\r\n\r\n").

%% 收件人地址
rcpt_to_emails(_Sock, []) ->
    ok;
rcpt_to_emails(Sock, [ToEmail | Rest]) ->
    send_socket(Sock, "RCPT TO <" ++ ToEmail ++ ">\r\n"),
    rcpt_to_emails(Sock, Rest).

%% send socket
send_socket(Sock, Data) when is_list(Data)->
    send_socket(Sock, unicode:characters_to_binary(Data));
send_socket(Sock, Data) when is_binary(Data)->
    io:format("Client: ~p~n", [Data]),
    ok = send(Sock, Data).

%% recv socket
recv_socket(Sock) ->
    case recv(Sock, 0) of
        {ok   , _W} -> io:format("Server: recv : ~p~n", [_W]), ok;
        {error, Reason} -> io:format("Server: recv failed: ~p~n", [Reason])
    end.

%% send data to server via tcp or ssl
send(Sock, Data) when Sock#socket.type =:= tcp ->
    gen_tcp:send(Sock#socket.sock, Data);
send(Sock, Data) when Sock#socket.type =:= ssl ->
    ssl:send(Sock#socket.sock, Data).

%% recv data to server via tcp or ssl
recv(Sock, Opinion) when Sock#socket.type =:= tcp ->
    gen_tcp:recv(Sock#socket.sock, Opinion);
recv(Sock, Opinion) when Sock#socket.type =:= ssl ->
    ssl:recv(Sock#socket.sock, Opinion).

test() ->
    send(#email{server_ip   = "smtp.host.com", %% "smtp.qq.com",
                account     = "youremail@host.com",
                password    = "yourpassword",
                subject     = "smtp邮件测试",
                content     = <<"您好,\r\n这是一封测试邮件，里面包含一组数字：0123456789。\r\n
                                Hello,\r\nThis is a test message that content numbers: 012456789"/utf8>>,
                to_emails   = ["yourtest@host.com"]
    }).
