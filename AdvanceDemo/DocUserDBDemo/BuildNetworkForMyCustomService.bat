rem 最简单的c4+dp+user模型

rem 这行脚本作用是启动DP服务器作为c4网络的种子
start /D .\ .\_1_Base_For_MyCustomService.exe "Title('种子服务器'),AppTitle('种子服务器'),Service('0.0.0.0','127.0.0.1','9199','DP')"

rem 这行脚本作用是启动UserDB服务器，并且向DP种子服务器报告自己的地址
rem 相当于user数据中心
start /D .\ .\_1_Base_For_MyCustomService.exe "Title('用户服务器'),AppTitle('用户服务器'),Service('0.0.0.0','127.0.0.1','9191','UserDB'),Tunnel('127.0.0.1','9199','DP')"

rem 这行脚本作用是启动Log服务器，并且向DP种子服务器报告自己的地址
rem 相当于log中心服务器
start /D .\ .\_1_Base_For_MyCustomService.exe "Title('Log服务器'),AppTitle('Log服务器'),Service('0.0.0.0','127.0.0.1','9192','Log'),Tunnel('127.0.0.1','9199','DP')"

rem 这几行脚本作用是启动_2_MyCustomService服务器作为VM服务器，并且向DP种子服务器报告自己的地址，同时，引用C4网络的UserDB+Log服务来使用
rem 这几行脚本相当于分布于各处的VM服务器，可以开很多很多
start /D .\ .\_2_MyCustomService.exe "Title('MyCustomService服务器1'),AppTitle('MyCustomService服务器1'),Service('0.0.0.0','127.0.0.1','9193','MyC4'),Tunnel('127.0.0.1','9199','DP|UserDB|Log')"
start /D .\ .\_2_MyCustomService.exe "Title('MyCustomService服务器2'),AppTitle('MyCustomService服务器2'),Service('0.0.0.0','127.0.0.1','9194','MyC4'),Tunnel('127.0.0.1','9199','DP|UserDB|Log')"
start /D .\ .\_2_MyCustomService.exe "Title('MyCustomService服务器3'),AppTitle('MyCustomService服务器3'),Service('0.0.0.0','127.0.0.1','9195','MyC4'),Tunnel('127.0.0.1','9199','DP|UserDB|Log')"
start /D .\ .\_2_MyCustomService.exe "Title('MyCustomService服务器4'),AppTitle('MyCustomService服务器4'),Service('0.0.0.0','127.0.0.1','9196','MyC4'),Tunnel('127.0.0.1','9199','DP|UserDB|Log')"
start /D .\ .\_2_MyCustomService.exe "Title('MyCustomService服务器5'),AppTitle('MyCustomService服务器5'),Service('0.0.0.0','127.0.0.1','9197','MyC4'),Tunnel('127.0.0.1','9199','DP|UserDB|Log')"
