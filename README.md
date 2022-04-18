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

