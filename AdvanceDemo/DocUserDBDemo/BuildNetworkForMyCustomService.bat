rem ��򵥵�c4+dp+userģ��

rem ���нű�����������DP��������Ϊc4���������
start /D .\ .\_1_Base_For_MyCustomService.exe "Title('���ӷ�����'),AppTitle('���ӷ�����'),Service('0.0.0.0','127.0.0.1','9199','DP'),Wait(1000)"

rem ���нű�����������UserDB��������������DP���ӷ����������Լ��ĵ�ַ
rem �൱��user��������
start /D .\ .\_1_Base_For_MyCustomService.exe "Title('�û�������'),AppTitle('�û�������'),Service('0.0.0.0','127.0.0.1','9191','UserDB'),Tunnel('127.0.0.1','9199','DP'),Wait(3000)"

rem ���нű�����������Log��������������DP���ӷ����������Լ��ĵ�ַ
rem �൱��log���ķ�����
start /D .\ .\_1_Base_For_MyCustomService.exe "Title('Log������'),AppTitle('Log������'),Service('0.0.0.0','127.0.0.1','9192','Log'),Tunnel('127.0.0.1','9199','DP'),Wait(5000)"

rem �⼸�нű�����������_2_MyCustomService��������ΪVM��������������DP���ӷ����������Լ��ĵ�ַ��ͬʱ������C4�����UserDB+Log������ʹ��
rem �⼸�нű��൱�ڷֲ��ڸ�����VM�����������Կ��ܶ�ܶ�
start /D .\ .\_2_MyCustomService.exe "Title('MyCustomService������1'),AppTitle('MyCustomService������1'),Service('0.0.0.0','127.0.0.1','9193','MyC4'),Tunnel('127.0.0.1','9199','DP|UserDB|Log'),Wait(6000)"
start /D .\ .\_2_MyCustomService.exe "Title('MyCustomService������2'),AppTitle('MyCustomService������2'),Service('0.0.0.0','127.0.0.1','9194','MyC4'),Tunnel('127.0.0.1','9199','DP|UserDB|Log'),Wait(7000)"
start /D .\ .\_2_MyCustomService.exe "Title('MyCustomService������3'),AppTitle('MyCustomService������3'),Service('0.0.0.0','127.0.0.1','9195','MyC4'),Tunnel('127.0.0.1','9199','DP|UserDB|Log'),Wait(8000)"
start /D .\ .\_2_MyCustomService.exe "Title('MyCustomService������4'),AppTitle('MyCustomService������4'),Service('0.0.0.0','127.0.0.1','9196','MyC4'),Tunnel('127.0.0.1','9199','DP|UserDB|Log'),Wait(9000)"
start /D .\ .\_2_MyCustomService.exe "Title('MyCustomService������5'),AppTitle('MyCustomService������5'),Service('0.0.0.0','127.0.0.1','9197','MyC4'),Tunnel('127.0.0.1','9199','DP|UserDB|Log'),Wait(10000)"
