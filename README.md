## ZNet是ZServer4D的技术重构和母体转移

#### 说说起源

ZServer4D在长期更新积累下，因为需要兼容许多项目群，导致了项目内部各种定义，命名，机制很难统一，在应用通讯技术到新领域时，这些不利因素都是障碍。

在2022年1月，我用机器化模型技术，对庞大的项目群，以及ZServer4D做了一次升级，简单来说，就是创造代码机器，然后大规模升级ZServer4D和项目群。目前，我已经完成了所有的编译和测试工作。

## ZNet的技术体系
#### 说说ZNet定位

ZNet在以后的主要定位是对SaaS，p2pVM，IOT，大数据存储IO（仅仅是IO），现役项目群，提供通讯地基支持。大家可以选择使用Pascal Rewrite Model技术直接把项目升级到ZNet，可以把ZNet直接重构成ZServer4D继续使用。

**ZNet是未来的ZServer4D母体，母体就是发源地，以后创造起源都会基于ZNet进行更新**，Pascal Rewrite Model技术可以让我们随时把最新的ZNet转换成ZServer4D或则别的分支项目，母体则是ZNet。

#### ZNet相比ZServer4D的大更新

```
新增C4-SaaS系统的XNAT支持
新增兼容fpc/delphi的文本分析技术
开始全面走代码规范化路线 diocp ics synapse 这些库 代码模型全部加了Z前缀 以后使用ZNet不再会与这些库发生冲突
提供大数据支持地基
针对IDC提供的基建服务部署C4，例如我们的服务器主机不在主干网 C4网络可以绑定一个ip 再侦听另一个IP
发送机制不再限制运行焦点必须主线程
客户端具备安全阻塞机制
高流量处理能力相比ZServer4D提升5倍向上
```

### 把ZServer4D项目群升级到ZNet
#### 主要使用代码重写技术：Pascal Rewrite Model，全自动化升级

- 代码重写模型简单来说，就是用机器代替了人类去规范化代码的技术，而规范化其实也就是人性化，更人性化表示更容易被长期提供支持和维护，因为pascal基本上都是35岁向上，工作的时间代价非常高，必须重视重复劳动
- 一个技术问题：**Pascal的编译机制决定了它Unit命名非常容易重复**，当三方Unit库文件达到1000数量级会高几率出现重复命名，Pascal Rewrite Model技术可以很好解决大规模并库问题
- Pascal Rewrite Model技术并不是ZNet专用，在我看来它可以辅助我们把项目优美的堆起来
- Pascal Rewrite Model技术可以正反双向重写代码：ZNet可以直接转换成ZServer4D，ZServer4D也可以直接升级到ZNet，其它项目依次类推
- **我会开源Pascal Rewrite Model技术，我的目标是帮助大家更好的做创造和创新，而不是借用新技术收割大家**
- 关于引导使用Pascal Rewrite Model，包括制作模型，应用模型，我已彻底开放这一技术体系，让每个人都能做到每天在几十万处diff中自由穿梭，不会局限于ZNet和ZServer4D互转

**把ZServer4D项目群升级到ZNet**

```
在 https://github.com/PassByYou888/ZNet/releases 找prp程序

把c:\myproj\目录的中所有delphi/fpc工程和代码统一升级成使用Znet库，包括.pas .inc .dfm .fmx .dpr .lpr
命令行格式 prp.exe "-D:c:\myproj\"

prp程序默认会打开并行方式，如果机器配置不够关闭并行，模型处理代码的时间更久
命令行格式 prp.exe -P:OFF "-D:c:\myproj\"

在升级后发现不习惯，或则不愿意使用ZNet，可以选择把c:\myproj\的工程和代码降级到ZServer4D
命令行格式 prp.exe -R "-D:c:\myproj\"
说明 -R 开关表示反向重写

依次类推，我们也可以使用prp把ZNet直接降级成ZServer4D然后在copy过去覆盖ZServer4D
命令行格式 prp.exe -R "-D:c:\ZNet\"
```

**编译prp程序**
```
先设置好ZNet的编译路径
prp程序位于 ZNet\Tools\PascalUnitRewriteTool
直接打开prp.dproj就可以编译了
PascalRewriteModel.dproj是prp的建模工具，都可以编译通过，本文档暂时不提供建模指南，请大家暂时自行摸索
后续我会专门开个项目讲解PascalRewrite模型的建模方法
```

**最新更新日志**

**2023-4-21 ZDB2解决PB级备份**

- ZDB2的备份技术分为静态备份,动态备份,自动化备份,三种模式,均为api调用方式触发,日常使用不会触发备份,下面的具体细节摘录来自代码
```pascal
  TZDB2_Backup_Mode =
    (
    // bmStatic is High speed IO backup, but there may be read-write data waiting for the backup queue to complete.
    // If the physical media is m2, nvme, ssd, they can be directly used
    bmStatic,
    // bmDynamicis is Slow and secure backup mode,
    // supporting level is TB/PB large-scale backup, suitable for hard drives with storage is HDD Group/Pool,
    bmDynamic,
    // bmAuto: When the data size > Static_Backup_Tech_Physics_Limit, use bmDynamicis, normal is bmStatic
    // TZDB2_Th_Engine.Backup_Mode default is bmAuto
    bmAuto
    );
```
- 近期忙于一个数据中心项目,数据量单日pb级,堆了3个hdd分组阵列,试运行第一天备份系统出现内存过载,直接宕机,第二天跟踪观察了一天,终于找到过载问题,修复!


**2023-4-11 新增p2pVM的底层加密机制**

- Z.Cipher库新增AES128,192,256,加密接口,完整集成,新增的AES体系可适用于ZDB2-Thread,ZNet,ZInstaller2,ZDB2-Encoder/Decoder等等技术
- 在Z.Net库p2pVM内核中新增Encrypt p2pVM Packet机制,可以避免p2pVM的明文数据暴露于公网,该更新从整体提升使用p2pVM体系项目的数据安全性
- Encrypt p2pVM Packet机制默认为关闭状态,建议通过修改Z.Define.inc中的Encrypt_P2PVM_Packet构建开关来启动p2pVM安全机制
- 提示:**Encrypt p2pVM Packet机制一旦启用,将会失去万兆网的支持能力**
- 在C4网络的启动脚本中新增文件替代功能:C4服务器的启动参数一旦多了,例如达到了2000字符长度的命令行,这是非常反人类的,因此c4给出了配置文件指向功能,当遇上长参数时,以配置文件来代替即可.


**2023-4-2 新增ZDB2数据集滚动存储支持**

- ZDB2的数据集由多个分散的小数据库组成,滚动存储支持机制为限制单库物理尺寸,达到以后做删头处理
- 新增ZDB2数据集原子条目的物理size属性,只针对数据集,zdb2底层无改
- Z.Net.pas核心库的机制无改

**2023-3-24 小更内核和zdb2数据引擎底层**

- 内核新增编译开关:LimitMaxComputeThread,这项开关影响深远,在内核中最大线程=cpuCount x 20,当线程数量超过以后,启动新线程将会做排队,等待前面的线程结束.这时,会出现一种挂起线程的情况,例如**Z-AI的DNN-Thread**技术,**Learn+IO-Thread**技术,**ZDB2的Thread-Queue**技术,线程启动后会等待,不会结束,当服务器配置不高,线程刚好沾满cpuCount x 20触发LimitMaxComputeThread,新线程执行而是一直等前面的线程结束,从而出现卡机现象.**当LimitMaxComputeThread,默认为关闭,就不会出现沾满cpuCount x 20现象了**
- 针对ZDB2新增自动化的备份还原机制,同时对ZDB2底层内核新增非正常关闭状态:数据库被打开时发现上次操作数据库是非正常关闭,将会直接触发备份还原机制,该机制只针对ZDB-Thread这一层以及上面构建的体系生效,不会对zdb2的api层生效


**2023-3-10,修复custom protocol支持机制bug**

- custom protocol支持机制流程无bug,但是泄露了收到0字节数据的ignore逻辑,某些迷之事件接收逻辑流程时数据长度为0,这时候,custom protocol支持机制认为有数据,然后往里面迭代
- 该bug非常不易发现,出现于"Z.Net.pas"核心库,导火索由群友qq346373实测xnat跑sql server发现不能连接,太太太粗心


**2023-1-6，新年开门红小更**
- 简单更新一些库命名
- 修复编译时error

**2022-11-27，大更新**

- **新增ZDB2高并发增删查改（单机）**
- **重大问题修复：新版本排序算法，彻底修复排序bug，全部测试通过**
- 提交了部分使用文档

**2022-10-30，大更新**

- **补充更新：新增高速配对算法Demo**
- **补充更新：安全阻塞**
- **补充更新：数据发送机制不再限制焦点主线程**，该更新对于安卓平台使用ZNet非常有用。**使用原则：仍以主线程为主**
- **解决android平台因为编译器造成的arc问题**，编译器兼容xe11.2，fpc3.2.x
- 重做流量均衡内核：底层大更，解决10W/s请求流量，造成服务器卡住的问题，该问题也类似cc攻击。该更新不影响现有服务群
- 重做底层hash系统：通讯能力将会出现1%小幅下降，兼容anroid的arc问题
- 新增一个C4 for android hello world的demo，IOS和OSX平台可同步
- 粘包机制大更新：不再使用copy方式切分数据，改用指针，可有效提升负载性能，同时也具有消化cc级请求的能力(其实大部分cc请求防火墙都可以拦截下来)
- 大家注意一下，ZNet已经被某些防火墙和操作系统平台加了数据跟踪线索，构建项目时尽量避免直接使用公共通讯参数，修改Z.Net.pas库中的部分命令和通讯参数可避免被跟踪
- 全部demo/tool编译测试通过，建议在线项目更新到该版本

**2022-10-9，应用项目修复**

- 实际应用项目中，发现许多问题，修复许多bug，改动非常多，无法一一叙述，核心机制无变化

**2022-6-19，置换设备后重启技术更新**

- hpc支持性大更新：分离出了物理网络数据和粘包流程，物理网络线程可处于无卡顿工作状态，解锁hpc高流量支持能力
- hpc支持性大更新：在无卡顿的物理网络机制中，由于缓冲技术需要使用内存，从而增加了一个磁盘暂存技术支持体系(使用ZDB2暂存)

```delphi
//hpc支持性使用说明，常量，直接修改Z.Net.pas文件

//BigStream每次发送的块大小，在万兆局域网中可以调大它，并发时它会很消耗内存，传大文件调大可提速
C_BigStream_Memory_SwapSpace_Activted: Boolean = False;
C_BigStream_SwapSpace_Trigger: Int64 = 1024 * 1024;

//在大量链接的服务器会用到主循环，该参数表示每次调用主循环的最大延迟，达到该延迟，循环会退出来，下次执行时会从检查点继续循环，默认100毫秒
//当服务器链接过多，且负荷不过来，该参能有效防止假死
TZNet.ProgressMaxDelay:TTimeTick

//启用或禁用BigStream磁盘缓存模型
C_BigStream_Memory_SwapSpace_Activted: Boolean = False;
//触发BigStream的缓存模型的条件，当发送队列的数据达到，就执行磁盘缓存（Stream必须是TMemoryStream，TMemoryStream64）
C_BigStream_SwapSpace_Trigger: Int64 = 1024 * 1024;

//启用或禁用CompleteBuffer磁盘缓存模型
C_CompleteBuffer_SwapSpace_Activted: Boolean = False;
//触发BigStream的缓存模型的条件，当发送队列的数据达到，就执行磁盘缓存，0表示全部缓存
C_CompleteBuffer_SwapSpace_Trigger: Int64 = 1024;

{ 在万兆网带宽峰值到来时，机器会处理不过来，这时候，缓存技术会把物理网络发来的数据记录到磁盘数据库，该常量是出发磁盘缓存的条件，当RX碎片达到100 }
C_Physics_Fragment_Cache_Activted: Boolean = False;
C_Physics_Fragment_Cache_Trigger: NativeInt = 100;
{ 缓存数据库，加密，步进尺寸 }
C_Swap_Space_Technology_Security_Model: Boolean = True;
C_Swap_Space_Technology_Delta: Int64 = 8 * 1024 * 1024;
C_Swap_Space_Technology_Block: Word = 1536;

//以上参数请自行摸索，hpc下的准万兆网支持模式还要考虑cpu调度，分线程，磁盘运用，数据库优化，多种因素决定结果
//并不是网络解决万兆项目就能跑出万兆效果

```

**2022-6-9，置换设备后重启技术更新**
```
针对hpc无法调试的修复更新: 在hpc使用cpuCount作为构建参数，程序启动就是300个线程，调试器会在启动时卡住
```

**2022-4-18针对ZDB2的革命性优化**

```
内核大更新，细节自行diff
优化Z.Net.pas库95%的链表数据结构，大规模负载时，内存消耗更小，而IO性能提升15%左右
ZDB2和C4相关服务，优化内核数据结构，针对大数据的增删插性能大约提升300倍
新增Hash泛型库
新增个demo：BigList vs List
ZDB2新增线程支持库
C4新增网盘VM框架
新增FS2 VM框架
新增TEKeyValue VM框架
新增Var VM框架
兼容Delphi XE7及以后版本，兼容FPC 3.x版本，支持FPC+ARM构建多平台
Tools, Adnvace demo, C4 Demo, Examples, 全部测试通过
```

**2022-4-8 兼容性更新**

```
感谢qq896616118,qq6192122,qq276678295, 提供无法编译信息，已修复兼容XE8及其以后版本，dcc+clang for OSX/ios/android未测试
```


**2022-4-6内核革命性大更**

```
本次大更全部集中于底层，对于上层应用群：大数据，AI，图像处理，网络，提供天然优化
重做线程池调度机制，重做Post Thread机制，重做TCompute线程触发机制，优化Z.Notify机制
Z.Core在内部新增一个支持64位地址段的泛型链表
重做MT19937随机数与线程实例匹配机制
DisposeObject调用不再会遍历互斥锁池，直接Free干掉
互斥锁池机制改为：UnlockObject(Obj)里面回收Critical句柄，释放内存
TCompute线程机制新增原子状态机变量：IsRuning，IsExit，完整兼容系统内核机制
synchronization机制（流程易错，效率超低），以后用状态机轻松回避它
新增ZDB2线程支持内核库：Z.ZDB2.Thread.pas
```

**过去的更新日志**

- 2022-3-24，新的并网机制：不影响之前的DP入网机制，已测试通过.
- 2022-3-24，新增网盘支持核心，已测试通过.
- 2022-3-16，给ZDB1.0的数据引擎增加了cache测试单元，因为减懒不写test case，所以疏忽，导致产生深度性质的cache bug.
- 2022-3-16，修复ZDB1.0的底层Cache：这个bug太难fixed了
- 2022-3-13，机制更新：C4入网不会再产生无用的链接：例如Depend Network包含DP，而目标网络没有DP服务，则不会产生链接，此机制更新对现有应用群无影响
- 2022-3-13，console app给出了help接口：当console启动时可以通过help查阅服务器和隧道的当前状态，另：所有的demo均使用console app模式
- 2022-3-13，修复DocUserDBDemo的启动问题
- 2022-3-13，优化DP接口：不再生成无用连接，即使生成链接，在默认5秒以后，也会kill掉无用的连接
- 2022-3-13，下个版本的ZNet将会支持网盘，更新幅度应该较大（大家放心，C4不会改机制，不影响现有应用群），本次更新的所有demo和库都测试通过，属于稳定版本（已经提交Release包）

3-13 机制更新说明: **GetOrCreatePhysicsTunnel/SearchServiceAndBuildConnection**
```pascal
// GetOrCreatePhysicsTunnel 入网以后不用再管了，有新服务器入网时它自动调度，离线也是自动处理
// 该方法需要保证C4网络有DP
C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel('127.0.0.1', 8888, 'dp|myService', nil);

// SearchServiceAndBuildConnection优于GetOrCreatePhysicsTunnel
// SearchServiceAndBuildConnection 是一次入网，不需要DP，不支持运行时调度
// 该方法只要C4网络有服务器标识符，都能入网
// SearchServiceAndBuildConnection 可以支持最优负载
C40_PhysicsTunnelPool.SearchServiceAndBuildConnection('127.0.0.1', 8888, 'myService', nil);

// 如果要支持运行时调度，只需要包含DP
C40_PhysicsTunnelPool.SearchServiceAndBuildConnection('127.0.0.1', 8888, 'DP|myService', nil);
```
3-13更新C4入网的命令行脚本: **AutoTunnel/Tunnel**
```
// C4脚本系统中的入网脚本，AutoTunnel方式优于Tunnel
// autoTunnel=SearchServiceAndBuildConnection方式入网，为一次性
// AutoTunnel(地址,端口,标识符,从服务器群选择负载最小的为false表示连上标识符为myService的全部服务器)
myClient "AutoTunnel('127.0.0.1',9188,'DP|myService',False)"

// Tunnel=GetOrCreatePhysicsTunnel方式入网，为一次性
// Tunnel(地址,端口,标识符最好带有DP，这样C4才可以有自动化机制)
myClient "Tunnel('127.0.0.1','9188','DP|myService')"
```

- 2022-2-13，修复了一个ZDB2重大bug：flush cache一个变量疏忽，导致写入错误的问题
- 2022-2-13，在AdvanceDemo中新增大文件备份演示
- 2022-2-10，qq375960048田华提了一个问题，dp在查询以后，客户端默认5秒才会断开服务器连接，这时候可能出现socket close信号无法到达服务器，导致客户端断开以后服务器连接仍然存在，今天主要修复这个问题：对服务器来说，凡事连接但p2pVM未创建都按5秒强断机制处理。
- 2022-2-10，近期qq345148965阿木/田华/qq860457224老廖，他们，正在应用ZNet，我或许会对他那边的需求做一些定制化的更新，大家尽请期待。

**文档篇幅有限，项目挺忙，暂时就写这么多，空了再为大家继续完善文档**
- by.qq600585
- 2022-1-19

