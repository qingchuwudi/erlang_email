%%%----------------------------------------------------------------------
%%% File    : email.erl
%%% Author  : qingchuwudi <privmail@foxmail.com>
%%% Purpose : send email
%%% Changed : 2016-3-8 by qingchuwudi <privmail@foxmail.com>
%%%
%%%
%%% erlang_email, Copyright (C) 2016   qingchuwudi
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

-export([start/0, send/1]).
-compile(export_all).

-define(MAX_SIZE, 1024).


%% send email by email record
%% 传参比原来复杂了
-spec send(tuple()) -> ok .
send(Email = #email{
    host     = Host,
    account  = Account,
    to       = ToEmail,
    password = Password,
    port     = Port,
    ssl      = SSL }) when
        undefined =/= Host,
        undefined =/= Account,
        undefined =/= ToEmail,
        undefined =/= Password ->
    ServerPort = case Port of
        undefined -> 
            case SSL of
                true  -> ?SSL_SERV_PORT_DEF;
                false -> ?NOT_SSL_SERV_PORT_DEF
            end;
        Any -> Any
    end,
    Sock = if 
        SSL ->
            {ok, Socket} = ssl:connect(Host, ServerPort,
                            [binary, {active, false}],
                            infinity),
            #socket{type = ssl, sock = Socket};
        true  ->
            {ok, Socket} = gen_tcp:connect(
                            Host, ServerPort,
                            [binary, {active, false}]),
            #socket{type = tcp, sock = Socket}
    end,
    connect_email(Sock, Account, Password),
    send_email_head(Sock, Account, ToEmail),
    send_email_info(Sock, Account, ToEmail, Email#email.subject),
    send_email_data(Sock, Email#email.content),
    end_email(Sock),
    case Sock#socket.type of
        ssl -> ssl:close(Sock#socket.sock);
        tcp -> gen_tcp:close(Sock#socket.sock)
    end.

%% 连接邮件服务器
connect_email(Sock, Account, Password) ->
   send_socket(Sock, <<"HELO ", Account/binary, "\r\n">>),
   recv_socket(Sock),

   send_socket(Sock, <<"AUTH LOGIN\r\n">>),
   recv_socket(Sock),

   send_socket(Sock, <<(base64:encode(Account))/binary, "\r\n">>),
   recv_socket(Sock),

   send_socket(Sock, <<(base64:encode(Password))/binary, "\r\n">>),
   recv_socket(Sock).

%% 邮件头，谁发送，发送给谁
send_email_head(Sock, Account, ToEmail) ->
    send_socket(Sock, <<"MAIL FROM: <" , Account/binary , ">\r\n">>),
    send_socket(Sock, <<"RCPT TO: <" , ToEmail/binary , ">\r\n">>).

%% 邮件信息，主题等
send_email_info(Sock, Account, ToEmail, Subject) ->
    send_socket(Sock, <<"DATA\r\n">>),

    send_socket(Sock, <<"FROM:<", Account/binary, ">\r\n">>),
    send_socket(Sock, <<"To: <", ToEmail/binary, ">\r\n">>),
    send_socket(Sock, <<"SUBJECT: ", Subject/binary , "\r\n">>).

%% 邮件内容
send_email_data(Sock, Content) when Content =/= undefined ->
    send_socket(Sock, <<"MIME-VERSION: 1.0\r\n">>),
    send_socket(Sock, <<"Content-Type:text/html;charset=UTF-8\r\n\r\n">>),
    
    ok = send(Sock, Content),
    send_socket(Sock, "\r\n\r\n");
send_email_data(_, _) -> ok.

end_email(Sock) ->
    send_socket(Sock, <<".\r\n">>),
    send_socket(Sock, "QUIT\r\n"),
    recv_socket(Sock).

%% send socket
send_socket(Sock, Data) when is_list(Data)->
    send_socket(Sock, unicode:characters_to_binary(Data, unicode, utf8));
send_socket(Sock, Data) when is_binary(Data)->
    ?DEBUG("Email Client: ~p~n", [Data]),
    ok = send(Sock, Data).

%% recv socket
recv_socket(Sock) ->
    case recv(Sock, 0) of
        {ok   , <<ErrCode:3/binary, _/binary>> = _Packet} -> 
            if
                ErrCode < <<"400">> -> ok;
                true ->
                    ?ERROR_MSG("Email Server: recv failed: ~p~n", [_Packet]),
                    throw({error, ErrCode})
            end;
        {error, Reason} -> 
            ?ERROR_MSG("Email Server: recv failed: ~p~n", [Reason])
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
    send(#email{
                host   = "mail_server_host",
                account     = <<"yourmail@somehost">>,
                password    = <<"password">>,
                subject     = <<"smtp邮件测试"/utf8>>,  % utf8 避免中文乱码
                ssl         = ture,
                content     = <<"您好,\r\n这是一封测试邮件，里面包含一组数字：0123456789。\r\n
                                Hello,\r\nThis is a test message that content numbers: 012456789."/utf8>>,
                to          = ["mailaddr@host"]
    }).
