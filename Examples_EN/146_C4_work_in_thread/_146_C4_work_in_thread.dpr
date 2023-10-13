program _146_C4_work_in_thread;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.DFE, Z.Parsing, Z.Expression, Z.Opcode, Z.Status,
  Z.Net, Z.Net.C4, Z.Net.C4_Console_APP;

type
  TMY_Serv = class(TC40_Base_NoAuth_Service);
  TMY_Cli = class(TC40_Base_NoAuth_Client);

procedure Second_Main(); {  Simulate dual main threads, this is the second mainline Main entry  }
var
  cli: TMY_Cli;
begin
  if C40_Extract_CmdLine(TTextStyle.tsC, {  Minimalist C4 framework for easy testing  }
    ['Service("0.0.0.0","127.0.0.1", 9000, "Serv000")', 'Client("127.0.0.1", 9000, "Serv000")']) then
    begin
      DoStatus('2nd main thread ID:%d', [Core_Main_Thread_ID]); {  The Z-series encapsulates the underlying thread synchronization mechanism and also defines its own flexible mainline model  }
      TC40_Auto_Deploy<TMY_Cli>.Create(cli); {  Upon seeing the dependent service readyok prompt, it indicates that the c4 network is ready  }
      C40_Execute_Main_Loop;
    end;
  C40Clean; {  There is no problem with ZNet running multithreading, and it has also been verified that the ZNet system threading model does not rely on the UI Synchronize mechanism  }
end; {  At this point, simulate the end of the main thread and return to the original thread state  }

begin
  DoStatus('1st main thread ID:%d', [Core_Main_Thread_ID]); // primary main thread
  RegisterC40('Serv000', TMY_Serv, TMY_Cli); {  To use C4, you need to register first. Serv000 represents the server architecture  }
  {  The first main thread is RTL, and the second main thread is Z-series  }
  {  Z-series dual main thread model, fully tested and approved, with good support for C4, SlossSocket, ICS8, ICS9, Indy, Synapse, DIOCP  }
  {  This model is very suitable for DLL, OCX, ActiveX, and branch services. All instances can support a dual line service, for example, attaching 10 dual line services only requires LoadLibrary to reach 10 times  }
  {  In DLL, Begin_Simulator_Main_The Second pointed by the Thread_Main is equivalent to the entrance of Exe  }
  {  The variables within the System system, including MainThreadID, are not involved, and the dual main thread model only targets the entire library in the Z system  }
  {  The dual main thread model does not support UI, and wherever CheckSynchronize, TThread. Synchronize are used, dual main threads are not supported  }
  {  If you want to forcefully support it, you need to connect to the program model: TSoft_Synchronize_Tool  }
  Begin_Simulator_Main_Thread(Second_Main); {  Using dual mainline technology requires opening the compiler to define the Core_Thread_Soft_Synchronize  }
  while Simulator_Main_Thread_Activted do {  After the simulation mainline ends, the thread environment of the Z-series will be automatically restored, and the dual mainline model has not modified any RTL variables  }
      TCompute.Sleep(1);
  Second_Main(); {  After the program execution is completed, it runs here to restore the thread environment and run C4 service again, this time in the first mainline space  }

end.
