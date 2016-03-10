-module(erlsmtp).

-export([
    send_email/0
]).

send_email() ->
    ssl:start(),
    % {ok, SslSock} = ssl:connect("mail.winext.cn", 465, [{active, false}]),
    {ok, SslSock} = ssl:connect("smtpdm.aliyun.com", 465, [{active, false}]),
    recv_msg(SslSock),
    ssl:send(SslSock, "EHLO GUIDAO\r\n"),
    recv_msg(SslSock),
    ssl:send(SslSock, "AUTH LOGIN\r\n"),
    recv_msg(SslSock),
    % Name = base64:encode("service@winext.cn"),
    Name = base64:encode("smartkit@mail.u0u.club"),
    ssl:send(SslSock, <<Name/binary, "\r\n">>),
    recv_msg(SslSock),
    % Passwrod = base64:encode("w23990916"),
    Passwrod = base64:encode("Winext123456"),
    ssl:send(SslSock, <<Passwrod/binary, "\r\n">>),
    recv_msg(SslSock),
    ssl:send(SslSock, <<"MAIL FROM: <smartkit@mail.u0u.club>\r\n">>),
    ssl:send(SslSock, <<"RCPT TO: <duweiguang@winext.cn>\r\n">>),
    ssl:send(SslSock, <<"DATA\r\n">>),
    ssl:send(SslSock, <<"From: <smartkit@mail.u0u.club>\r\n">>),
    % ssl:send(SslSock, <<"From: <service@winext.cn>\r\n">>),
    ssl:send(SslSock, <<"To: <duweiguang@winext.cn>\r\n">>),
    ssl:send(SslSock, <<"Subject: 测试邮件\r\n"/utf8>>),
    ssl:send(SslSock, <<"MIME-version: 1.0\r\n">>),
    ssl:send(SslSock, <<"Content-Type: text/html;charset=UTF-8\r\n\r\n">>),
    % ssl:send(SslSock, <<"<html><body>测试邮件</body></html>\r\n">>),
    ssl:send(SslSock, <<"测试邮件\r\n\r\n"/utf8>>),
    ssl:send(SslSock, <<".\r\n">>),
    ssl:send(SslSock, <<"QUIT\r\n">>),
    recv_msg(SslSock),
    ssl:close(SslSock).


recv_msg(Sock) ->
    case ssl:recv(Sock, 0) of
        {ok, Data} ->
            io:format("data:~p~n", [Data]),
            {ok, Data};
        _E ->
            io:format("error:~p~n", [_E])
    end.