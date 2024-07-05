## ZNet是ZServer4D的技术重构和母体转移

### ZNet的用途

在服务器领域,通讯功能已经非常普及,这时候,侧重面变得更偏向服务器的能力,ZNet体系站在计算机机理立场下,以结构和算法作为辅助,从ZDB2大数据体系+现代化泛型结构体系+先进线程控制体系,全方面提升服务器工作能力的框架.

**对ZNet来说通讯系统只是一个接口,这并不重要,服务器里面的运作才是关键**.例如:

- web的数据处理中心,使用ZNet以web-api来支持现代化项目体系.
- 在IOT,AIOT内部集成通讯系统
- 创业型项目涉及到大量通讯,满世界没有资源,自己动手开始diy项目,就可以试试ZNet
- 超越了常规的大数据存储和分析项目,数据量达到pb级
- 小型通讯化的工具,例如文件上传下载管理,内网穿透,自动化部署
- Z-AI第六代监控的数据中心后台使用ZNet解决

关注和使用ZNet前,需要做好大量编程,深入数据结构,以从计算机机理角度解决业务落地的准备工作.


#### 说说起源

ZServer4D在长期更新积累下,因为需要兼容许多项目群,导致了项目内部各种定义,命名,机制很难统一,在应用通讯技术到新领域时,这些不利因素都是障碍.

在2022年1月,我用机器化模型技术,对庞大的项目群,以及ZServer4D做了一次升级,简单来说,就是创造代码机器,然后大规模升级ZServer4D和项目群.目前,我已经完成了所有的编译和测试工作.

## ZNet的技术定位

- ZNet认为编程就应该遵重计算机理,ZNet不玩概念和设计,一切问题都用最朴实的办法解决:算法+机理+API=solve
- ZNet设计初衷就是注重编程,并且提供了堆大项目的C4
- ZNet更新很频繁,大部分是新增功能,fixed bug频率极小
- 不搞技术噱头,ZNet所有solve信息全部会与项目落地发生对应
- pas圈的尖端网络地基,请勿与https,web这类体系相提并论,ZNet偏向群集,算力,部署,结构,设计模式等等方向.

ZNet的主要定位是对SaaS,p2pVM,IOT,大数据存储IO,现役项目群,提供通讯地基支持

详见参考手册 https://zpascal.net/ZNetManual.pdf 

#### ZNet相比ZServer4D的大更新

- 真正支持n块万兆网卡有效负载,日吞吐量可上PB级
- 内置现代化数结构和算法支持体系,包括跨越NUMA的线程体系支持,协程体系支持,相比TList提速10倍以上的专属结构化链表体系,结构配对器,结构Hash容器体系
- ZDB2+ZNet在idc机房级项目有完整支持体系
- C4-SaaS体系支持
- ZNet内置的三方库均为全面重构+fixed后的专属版本
- 针对IDC提供的基建服务部署支持
- 通讯内核全面支持多线程,可自己仿真阻塞,封装通讯协议
- 高流量处理能力相比ZServer4D提升20倍向上
- 用fpc构建的通讯程序在IOT平台也可以具有超高流量和大型服务器架构能力

## 目标架构可执行测试与网络驱动

- dcc = dcc编译器支持该三方通讯库
- fpc = fpc编译器支持该三方通讯库
- n/a = dcc/fpc编译器均无法支持该三方通讯库

| 测试架构   | Cross | synapse | indy | ICS | ICSV9 | DIOCP |
| ------- | :------: | :------: | :------: | :------: | :------: | :------: |
| x86  | dcc | dcc | dcc | dcc | dcc | dcc |
| x64  | dcc | dcc | dcc | dcc | dcc | dcc |
| ARM  | fpc | fpc | fpc | n/a | n/a | n/a |
| MIPS  | fpc | fpc | fpc | n/a | n/a | n/a |

## 多系统的通讯接口建议

- RS = 服务端支持该平台并建议采用
- RC = 客户端支持该平台并建议采用
- CR = 支持该平台但只作为候选建议
- CC = 支持该平台但只作为候选建议
- NR = 服务端不支持该平台
- NC = 客户端端不支持该平台

| 测试架构   | Cross | synapse | indy | ICS | ICSV9 | DIOCP |
| ------- | :------: | :------: | :------: | :------: | :------: | :------: |
| linux(FPC)  | RS/CC | CR/RC | NR/NC | NR/NC | NR/NC | NR/NC |
| win7  | RS/RC | CR/CC | CR/CC | CR/CC| CR/CC | CR/CC |
| win10  | RS/RC | CR/CC | CR/CC | CR/CC| CR/CC | CR/CC |
| win2019  | RS/RC | CR/CC | CR/CC | CR/CC| CR/CC | CR/CC |
| win2022  | RS/RC | CR/CC | CR/CC | CR/CC| CR/CC | CR/CC |
| 国产L(FPC)  | CR/CC | RS/RC | NR/NC | NR/NC | NR/NC | NR/NC |
| Android  | RS/RC | NR/NC | CR/CC | NR/NC | NR/NC | NR/NC |
| OSX/IOS  | NR/NC | NR/NC | RS/RC | NR/NC | NR/NC | NR/NC |


### 关于项目堆大

很多开源项目看似都很简单易懂,在真实的服务器项目中,规模也许会比开源项目大上100倍.从量变到质变,规模上升100倍,就不再是常规技术方案了.

很多人做服务器是调度数据库+通讯系统,如果是做web,服务器还会包含设计ui系统.如果项目规模很小:通讯频率低+通讯数据量小,这种服务器怎么做都没有问题.
	
当服务器规模开始偏大:通讯频率高+通讯数据量大,将会面临:围绕硬件编程.

6核12超线和128核256超线,这在硬件定位上是不一样的,它会影响线程和服务器的设计模型,6核规格硬件几乎无法开出并行程序模型,那怕一个环节开出并行for,也许整个服务器都会受牵连,6核的只能开出常规线程对特别繁重的计算环节跑线程分载.当服务器运行于128核平台时,就没有cpu计算资源的问题了,而是合理安排计算资源,例如,4线程的并行计算,有时候会比使用40个线程速度更快.最后是系统瓶颈,在不使用三方线程支持库情况下,win系统的单进程最多只能同时使用64个超线,只有挂载了TBB这类库以后,才能同时使用256个超线.全线程,部分线程,单线程,这种服务器在设计之初定位就已经完全不一样.服务器程序的中心是围绕不同时代的硬件趋势,最大的道理硬件平台,框架设计环节是小道,只有明确了围绕硬件为中心,再来做服务器设计和编程,这才是正确的路线.

再比如8张4T全闪sdd配192G内存,以及8张16T的hdd配1TB内存,这种服务器在数据存储,缓存系统设计上也是不一样的,例如数据规模到30亿条,空间占用到30T,这种规模基本上192G内存会很吃紧,但不是问题,仔细优化后也能跑,因为数据要加速搜索或存储提速永远都是用缓存,而当存储设备的硬件配置使用hdd并且容量达到100T,内存1TB,这种设计将比192G更加需要优化缓存,流量进来以后有可能光是写缓存就直接吃掉0.99TB内存,程序在设计之初就已经定位好了用10G来hash索引,开各种优化算法的结构,这2种不同硬件配置,在服务器的系统设计层面,是不一样的:192G只需要考虑优化缓存规模,1TB需要考虑优化缓存规模+防止崩溃,因为hdd写大数据遇到流量>阵列写入极限后非常容易崩溃,大阵列的内存一旦用完,阵列的IO能力也许会下降到原有能力的5%,与崩溃无差异,缓存控制(flush)将会是直接提上前台的核心机制之一,这需要从整体上控制住大数据输入端,阵列写机制,硬件锁这些关键要素.

最后是gpu,一旦服务器碰上gpu,支持设备从cpu到存储几乎全都会是高配,这时候,服务器在程序设计环节将会彻底脱离古典的单线程方式,流程模型将会被流水线模型所取代,这些流水线会从一个作业系统到另一个作业系统流来流去.这时候ZNet程序会全体走Buffer+线程的路线,并且这种模型只是把数据接进来,计算主体是一堆独立+巨大的计算支持系统.

详见参考手册 https://zpascal.net/ZNetManual.pdf 


### 把ZServer4D项目群升级到ZNet
**主要使用代码重写技术：Pascal Rewrite Model,全自动化升级**

- 代码重写模型简单来说,就是用机器代替了人类去规范化代码的技术,而规范化其实也就是人性化,更人性化表示更容易被长期提供支持和维护,因为pascal基本上都是35岁向上,工作的时间代价非常高,必须重视重复劳动
- 一个技术问题：**Pascal的编译机制决定了它Unit命名非常容易重复**,当三方Unit库文件达到1000数量级会高几率出现重复命名,Pascal Rewrite Model技术可以很好解决大规模并库问题
- Pascal Rewrite Model技术并不是ZNet专用,在我看来它可以辅助我们把项目优美的堆起来
- Pascal Rewrite Model技术可以正反双向重写代码：ZNet可以直接转换成ZServer4D,ZServer4D也可以直接升级到ZNet,其它项目依次类推
- **我会开源Pascal Rewrite Model技术,我的目标是帮助大家更好的做创造和创新,而不是借用新技术收割大家**
- 关于引导使用Pascal Rewrite Model,包括制作模型,应用模型,我已彻底开放这一技术体系,让每个人都能做到每天在几十万处diff中自由穿梭,不会局限于ZNet和ZServer4D互转

**升级操作方法**

```
在 https://github.com/PassByYou888/ZNet/releases 找prp程序

把c:\myproj\目录的中所有delphi/fpc工程和代码统一升级成使用Znet库,包括.pas .inc .dfm .fmx .dpr .lpr
命令行格式 prp.exe "-D:c:\myproj\"

prp程序默认会打开并行方式,如果机器配置不够关闭并行,模型处理代码的时间更久
命令行格式 prp.exe -P:OFF "-D:c:\myproj\"

在升级后发现不习惯,或则不愿意使用ZNet,可以选择把c:\myproj\的工程和代码降级到ZServer4D
命令行格式 prp.exe -R "-D:c:\myproj\"
说明 -R 开关表示反向重写

依次类推,我们也可以使用prp把ZNet直接降级成ZServer4D然后在copy过去覆盖ZServer4D
命令行格式 prp.exe -R "-D:c:\ZNet\"
```

**编译prp程序**
```
先设置好ZNet的编译路径
prp程序位于 ZNet\Tools\PascalUnitRewriteTool
直接打开prp.dproj就可以编译了
PascalRewriteModel.dproj是prp的建模工具,都可以编译通过,本文档暂时不提供建模指南,请大家暂时自行摸索
后续我会专门开个项目讲解PascalRewrite模型的建模方法
```

## 最新更新日志

**重大更新:2024-7-5 OpCode体系新增非线性支持机制**

- OpCode属于内核库,本次更新已经尽最大努力兼容老代码
- zExpression Cache机制调整:旧机制直接从cache池获取已编译好的OpCode使用,新机制调整为:从cache池获取已编译好的OpCode,这时候会copy一个OpCode副本,待OpCode运行后会释放该副本.
- 移除OpCode中的Trigger机制:当传递方式在调用api前,会给trigger变量赋值当前OpCode实例(api运行焦点),当共用OpCodeRunTime实例时,将会导致多线程的安全问题.顾直接移除该机制.
- 大幅更新:Z.Expression.Sequence.pas库内部工作机制的从物理堆栈运行模式更新为使用OpCode的非线性体系模式,TExpression_Sequence在app层使用方式保持不变.
- Z.Expression+Z.OpCode+Z.Expression.Sequence,提升对线程的支持,提升运行于高并发后台的安全性.
- TOpCode_NonLinear对于支持vector expression说明:如果脚本中函数有a(),b(),这种vector表达式TOpCode_NonLinear不被直接支持的,需要使用TExpression_Sequence或TOpCode_NonLinear_Pool框架.
- 由于使用TOpCode_NonLinear需要主循环框架,因此内置了喊主循环的多线程支持体系TOpCode_NonLinear_Pool
- TOpCode_NonLinear体系设计是闭环的.没有设计遗漏.对于多线程并发和传递的支持度良好.
- 新增TOpCode_NonLinear体系是追加模式,这也是新一代表达式地基,同时兼容老代码.
- 全部demo已经编译测试通过.


**2024-6-25 新增序列化脚本app-Demo**

- 这是一种系统集成自动化的app程序模型,不是具体某个库,也不是某个算法,这是一种体系
- 序列化脚本app可以解决了系统集成中的许多自动化环节
- 这是尖端型的自动化应用思路

**2024-6-22 新增时序大数据Demo**

- 该Demo是时序大数据的应用级结构算法,见Examples/161.Hash_Time

**2024-4-23 小规模更新**

- 新增一个有趣的PI精度计算程序,可以极快方式计算到double极限精度
- expression非线性流程在实际项目应用,修正了许多细节
- 修正延迟释放状态输出方式:在C4命令行使用Enabled_Delay_Info or Delay_Free_Info启用延迟释放信息
- C4命令行中的exit命令会优先关闭Instance Tracker,然后才会执行关闭,**因为finalization块中许多释放机制会遇到某些上百万的instance tracker信息,在关闭时如果输出上百万条free object会非常卡**

**2024-4-3 小幅度更新**

- 新增console help for UI,该demo演示了在UI界面使用c4的命令行工具,内核状态,延迟释放,ZNet实例,命令执行开销,这些是最基本的服务器和客户端运行状态分析.
- 更新C4的入网connected事件机制,connected事件触发时必然在主线程
- 更新TPeerIO中的UserDefine和UserSpecial保护释放规则:当Busy和BusyNum处于非重置状态中,会保护实例,从而达到同步线程的作用.更新后的机制为,当Busy和BusyNum被标记后,Owner实例会重置为nil,表示IO已经失效,但UserDefine和UserSpecial依然存在,必须直到Busy和BusyNum被重置后才会释放,内部工作原理是在释放IO时检测Busy和BusyNum,如果发现正在使用,就开辟一个子程不断的检查Busy和BusyNum.
- 修改预编译定义 **Intermediate_Instance_Tool** 默认为打开状态,内核会记录所有实例的创建和销毁状态
- 修改预编译定义 **Tracking_Dealy_Free_Object** 默认为打开状态,内核在延迟释放对象时,会抛出信息,这条信息,必须在c4中以命令行执行Delay_Free_Info来开启.

**2024-3-28 新增IO方法Progress_IO_Now_Send,优化XNAT**

- Z.Cipher库新增文本加解密函数
- Z.Net库新增Progress_IO_Now_Send方法
- XNAT实时传输优化,在XNAT侦听端口的数据到达时,不再仍队列,而使用Progress_IO_Now_Send直接转发到目标地址,效率提升大约10%
- 修正XNAT内部变量和方法命名

**2024-3-13 新增序列化表达式,解决非线性流程问题**

- 非线性流程可配置化,可脚本化一直都被大量应用,但是,一直很少规范化它
- 本次更新给出了规范化的序列化表达式以及Demo
- Distance浮点计算新增SSE支持
- 更新Z.Define预编译编译文档


**2024-1-23 主要补充了一些文档(中文)**

- 更新细节下次来补充

**2023-12-28 微泄漏检测技术支持**

- 修正时间跨度算法内部的内存回收机制(Z.HashMinutes.Templet.pas, Z.HashHours.Templet.pas)
- 全部基类和泛型结构一律具备实例跟踪机制
- C4框架集成微泄漏分析跟踪功能
- 补充微泄漏支持的文档

**2023-11-21 C4适应性入网**

- C4新增支持自适应地址转换:自动化自适应单服务器C4模型,可以在单台服务器开n个C4服务,无需管理IP地址,直接入网
- 修正CPM群集侦听问题:CPM默认侦听使用上报IP,修正后使用C4的侦听IP,这是一个算法上的疏忽
- 修正bug:正在传输文件时,NoAuth客户端双通道被释放,无法关闭文件问题
- ZNet手册文档更新

**2023-11-14 修正CPM群集系统自动适应性问题**

- CPM群集系统在新增地址mapping以后,需要自动协调,而CPM群集的自动协调出现了一处bug,导致运行中新增地址mapping无法使用
- 修复后:后台服务器进入稳定模式后,CPM走自动协调路线,无需人为干扰

**2023-11-8 修正ZDB2的滚动存储机制,更新XE12内核支持**

- ZDB2的滚动机制为定时rolling,删头加尾,在大数据后台 "Remove_First_Data_For_All_Th_Engine" 操作,大约延迟5分钟,其工作机制为:先锁数据库,然后删头增尾,由于批处理延迟,在5分钟内的数据全部暂存在内存中,这时可能出现内存耗尽引发崩溃
- 修正后的机制:当大数据库达到极限规模时,每次新增单数据条目,直接删头增尾(在内核进行操作),不再定时使用 "Remove_First_Data_For_All_Th_Engine" 做批处理
- 支持XE12:Swap符号被内部定义了,在Z.Core定义的swap不让过,故移除全部Swap编译符号,改用"TSwap<Integer>.Do_"操作Swap
- XE12全部测试通过
- FPC3.2.x版本全部测试通过


**2023-10-17 修复严重bug,新增群集端口代理(IDC机房使用的代理技术)**

- 严重bug修复:通讯组件释放时可能出现死循环爆堆栈:通讯组件释放时主要使用次主循环同步规则检查线程,次主循环没有关闭事件接口,可能会导致c4在频繁创建+释放物理接口时,出现循环套循环从而爆堆栈
- 严重bug修复:在Z.Geometry2D库,对称到飞对称投影计算漏掉了第二个坐标,症状为:非对称投影框框走样,因为从来都是fitrect来画图,非对称api极少使用,导致计算遗漏
- 新增XNAT群集代理技术,基于C4框架,目前来看3行一套CS的C4框架是写demo的首选
- 新增英文版demo+文档,繁体版还在考虑,有空会补充
- 今天发现许多地方有命名修正需要,有空会解决

**2023-10-11 新增性能分析工具箱**

- 近期大更提出了双主线概念,主循环机制被摆上前台,因此给出性能分析工具箱,让寻找主循环的cpu消耗卡顿点变得可行
- 修复C4Progress主循环中的重叠执行问题
- 在ZNet手册同步更新了性能分析工具箱的操作方法

**2023-10-10 修正全部demo**

- 由于更新了底层机制,导致部分demo无法使用
- examples目录中的小部分demo接近重写,例如多线程demo
- 底层机制修正: 重要的事情说三次, **必须自己在主循环里面CheckThread,物理通讯不再调用CheckThread**
- 底层机制修正: 重要的事情说三次, **必须自己在主循环里面CheckThread,物理通讯不再调用CheckThread**
- 底层机制修正: 重要的事情说三次, **必须自己在主循环里面CheckThread,物理通讯不再调用CheckThread**
- 移除物理接口CheckThread因为关系到双主线程技术体系,因此会有1ms延迟,主循环如果不管会出现UI卡顿
- 主循环还需要进一步优化工作,等这几天稍微缓一下!


**2023-10-9 新增线程支持体系**

- 新增双主线程技术支持体系
- 重新梳理了cross/diocp/ics8/ics9/synapse/indy的通讯接口,全部test passed.
- cross新版本还没有正式接入,缓冲几天
- 新增双主线程的服务器demo,位于Examples目录
- 更新技术手册

**2023-10-7 ZNet立项以来的最大更新**

- 全面万兆以太支持
- 重做Synapse通讯C端,fpc+laz+znet+synapse,这套组合已具备项目应用能力
- 全面解决ZNet在fpc平台的可移植性
- 新增万兆网的通讯机制Demo,位于Examples目录中
- 新增ICSV9支持,目前ICS8与ICS9在Znet重叠,ICSV9库标头以Z.ICS9命名,ICSV8库标头以Z开头命名,以此区分
- 关于CrossSocket,CrossSocket的fpc版本支持还在路上,目前测试中,未来不定期更新


**2023-9-23 数据中心出现了宕机,紧急修复,新增写阵列盘的等待参数**

**问题发生**

- 跑hdd的阵列系统读写大文件时会有寻道时间,当ZDB2的数据库文件体积达到20TB,空间表按碎片方式排列,覆盖写入空间表时会出现更加耗时的寻道机制
- ZDB2单次flush数据量也许只有几百M,这并不大,但这几百M的数据刚好遍布在20TB文件各个位置
- 因为等flush太久,传导给了暂存系统,内存和临时硬盘空间告急,最终反应在操作系统的症状:系统内存196GB,程序使用了160G,这时候,系统已经崩溃,所有alloc函数全部出错.
- 因为flush寻道太久,系统内存强制占用30G++,并且标记为不可用内存,当程序达到了160G,系统资源耗尽,另一方面系统保护程序给的临界值为190G,该问题直接绕过保护程序,最终导致服务器崩溃

**解决**

- 首先,ZDB2如果是大数据存储项目,需要避免一下分散空间表,尽量在一个区域,也许这些空间表数据会达到数千M,这样干,hdd阵列会有一定占比量连续性写入机制,主要看内存体量大不大.
- 其次,ZDB2的高级框架,会产生几十个ZDB2数据,而ZDB2这些数据库单体也均达到10TB,flush间隔写入时间为5分钟,在这5分钟时间,所有写入数据均暂存于内存
- 当flush触发时,阵列写入瓶颈会拉满600M/s,这时候,如果使用WaitFlushDisk这类api,有可能会等很久,导致破坏线性程序模型,这时候,就不能再使用waitflush机制了.
- **本次宕机问题主要是写入能力不够快,在实际idc机房中,阵列可以配置多个系统,把ZDB2分散出来存**.

**关闭waitflush机制后,传导出的新问题**

- 如果不使用waitflush,那么每次flush就无法保证单步还原:IO读写分离系统是先建立一个恢复文件,再对阵列做flush,如果不等flush,就无法保证恢复文件能成功还原.
- ZDB2的还原机制为序列化覆盖还原,1小时内的所有写数据,全部暂存起来,断电后进行大批量还原.
- 内部ZDB2的还原模式会根据waitflush机制是否有效进行自动切换,无需人为干预.
- 至此大数据在阵列宕机问题已solved.


**2023-9-11 解决:在IO读写分离以后,写入变慢,引发ZDB2跑阵列系统带宽不够**

**问题发生**

- ZDB2有一个空间分配表,当ZDB2的单库达到100TB极限时,单库空间表会逼近2GB,这时候,100个库使用flush方法回写空间表,将会出现200GB的瞬间写入流量
- 当flush按每5秒持续回写,将会卡住异步线程中的command-list请求队列

**解决**

- 分割写入:只对发生了修改的分配表做回写操作,先做初步的动态表判断,如果满足条件,把空间表切割出来,只写入修改的数据,最终阵列带宽降低非常明显!
- 写缓存优化:在IO读写分离结构中,给出了新的优化跨度算法,这是一种范围型的hash方法,表示哪些范围被写入过数据,缓冲底层IO请求,例如,1个小时不做flush操作,内存中留有1000万个写数据,高速复写!
- 更多的SafeFlush:读写IO分离机制,将生成更多的写缓存备份,只要阵列系统表没有丢,ZDB2就可以完成恢复.
- SafeFlush恢复机制调整:不使用全回写方式恢复,而是最后的分离写入数据,例如在10个SafeFlush中,只会选择最后的一个分离写入恢复,假如最后一个SafeFlush坏掉,就往上推.


**2023-9-4 解决ZDB2在断电时的数据损坏问题**

- 底层新增非连续性碎片缓冲支持库,这是一个离散的2D数据拼接算法:当写入数据时,会在底层重构一次碎片结构,再当读取时,会从碎片结构合并,这样就做到了读写媒介分离,读一个媒介,写则用碎片结构,该支持库用于ZDB2断电保护支持
- ZDB2全系支持断电保护,所有对于硬盘的写,一律读写分离,同时ZDB2的IO效率也会下降10%
- 修复ZNet对于XE7的DCC支持


**2023-8-26 毫无人性的大规模命名修正**

- 无法忍受许多含糊不清命名
- 老项目更新正需要prp模型,在PRP位于发行包中,老工程使用prp命令行移植,兼容ZServer4D老工程
```sh
	rem 一定要使用新版本的PRP老版本内部不包含本次更新的命名模型
	prp.exe "-D:your project directory"
```
- Z.Net.pas库中的所有IO都可以直接推算出发送和接收通道
```pascal
	TPeerIO
	{ double tunnel }
	function Is_Double_Tunnel: Boolean;
	function Is_Recveive_Tunnel: Boolean;
	function Is_Send_Tunnel: Boolean;
	function Is_Link_OK: Boolean;
	function Get_Send_Tunnel: TPeerIO; overload;
	function Get_Send_Tunnel(var Send_Tunnel: TZNet; var Send_Tunnel_ID: Cardinal): Boolean; overload;
	function Get_Receive_Tunnel: TPeerIO;
	function Get_Recv_Tunnel(var Recv_Tunnel: TZNet; var Recv_Tunnel_ID: Cardinal): Boolean; overload;
	end;
```
- HPC线程会自动检测是否处于双通道模型,并且自动推算出发送和接受通道
- 新增三角镜像函数

**2023-8-9** 新增一个demo:long run,这是一个被大量使用,但一直没有demo的长处理机制

- long run机制可以在服务端简单实现大规模并行计算处理,且不会卡队列
- long run机制是一种编程范式,无法做进ZNet主库

**2023-8-5 一个基于ZDB2的具体大数据框架**

- 新增一个大数据框架,由于需要光栅技术支持体系,ZNet没有demo,而是把集成到了Z-AI1.4项目[地址](https://github.com/PassByYou888/Z-AI1.4/tree/main/AI-demo/_138_C4_LargeDB_Picture_Label)

**2023-7-20 从idc运营项目中剥离了webapi技术方案,作为demo使用**

- 目前webapi只有http-get方式,并且只有一个函数get_demo_info,后台请求流程属于完整方案,可直接套用外部语言,webapi位于AdvanceDemo目录
- **webapi说明:webapi后台服务我们一次开了数十个,并且配有进程守护程序,系统守护程序,numa+亲和性调度系统,这次提交是webapi服务本体,代码较多,运营级项目使用**
- 新增3个小demo,位于Examples目录

**2023-5-28 C4入网机制优化**

- **解决服务器群和IOT设备群的重启后的入网问题,C4的大规模群集部署将会更加自动化**
- 因为C4在入网前连接都是不安全的,断线既故障,C4只有在入网后,才会启动断线重连机制
- 这就导致了C4服务器群一旦重启,被C4连接的服务器如果未就绪,那么C4将会部署失败,这时候需要人为介入操作
- 本次更新对C4网络新增 **ZNet_C4_Auto_Repair_First_BuildDependNetwork_Fault** 开关,打开以后,C4首次入网失败时将会以5秒间隔反复尝试重连,持续4小时
- **主要是作者的数据中心项目经常重启服务器,作者很懒,不想每次都人为介入,懒人没办法**

**2023-5-20 ZDB2大循环保护机制**

- 我在实际应用项目中发现:100个300万级以上的大循环遍历任务有点耗时,且遍历中,容易发生数据插队,遍历数据还没走完,新数据又来了
- ZDB2给出了大循环保护机制:在大循环遍历中,数据会存入磁盘,但不会进入遍历的数据链实例,数据可从磁盘删除,但不会从实例链移除,妥妥的大循环稳定性.
- 本次更新可支持开子线程+一个for走完百亿条目遍历,大数据处理效率+人性化=项目可堆大可行性
- **本次更新解决大循环时间变量中的不确定性事件影响,线程中的大循环可能会走1小时,中间会发生太多数据不确定性,这是一个数据库设计层面大难题,已解决**

**2023-5-11 许多小更新**

- 我在实际应用项目中发现的基础库小问题,顺手修复
- 在我使用了ZNet,C4,ZDB2这类技术的项目中,涉及到了多大型机柜规模的服务器群,分组阵列,万兆光纤直连等,目前整体表现非常稳定

**2023-4-29 修复一个颤抖级bug**

- 该bug问题出现于阵列备份系统中,是人类及其容易疏忽的32位表达式问题,例如seek一个64位文件位置,表达式为int32(a)+int32(b),编译器认为这是一个32位数值换算,给出的结果为32位,丢失高位
- 该bug出现于近几天编写的新算法中,用调试器几乎无法察觉,发现服务器报seek error异常,感觉出在地址数值,直接翻译成汇编一看,果然,这真是一个让人颤抖级的bug啊

**2023-4-28 小更新**

- Z.Net通讯库新增一个等反馈机制开关(**AutomaticWaitRemoteReponse**)
- 当使用SendDirectConsole,SendDirectStream,SendCompleteBuffer,数据流会一直向目标IO发送,这是无反馈的发送机制.网络不畅时会导致了大量数据堆积在发送缓冲区,AutomaticWaitRemoteReponse开关用于控制对CompleteBuffer这类发送机制的远程响应,最终会形成如下模型
- **AutomaticWaitRemoteReponse** 被打开后,CompleteBuffer来不及送出的数据会被磁盘缓存,然后按队列逐条发送出去,使用CompleteBuffer不会出现全部堆发送缓冲区
- **AutomaticWaitRemoteReponse** 被打开后会失去万兆以太的支持能力, AutomaticWaitRemoteReponse 适用于有物理带宽限制的云服务器
- 将CrossSocket通讯接口中的send机制调整成多线程模型,过去在发送队列中的续发为sync到主现成干,调整后,变为直接在线程中续发
- 小幅加固CrossSocket处理断开io后的事件

**2023-4-26 小更新**

- 主要加固底层的文件api支持,昨天我在数据中心启动了一个10tb规格的备份任务时,中途出现了seek error, 只一瞬间,当时,我强制stop_backup,并没有造成数据丢失,晚上思来想去,还是给底层api库做了一个加固工作
- 目前zdb2支持的大数据系统良好运作中,一个系统进程带了3个pb级数据量的分布式zdb2数据库,每个库带10-20个子库,硬件一律架构与大型分组阵列


**2023-4-24 更新2处地方,ZNet-C4网络拓扑结构补丁+ZDB2在多种阵列系统下的快速文件访问支持**

- 问题起源于一个懒人做法:不想把原有使用C4的服务器端以C4-VM做替代.(**C4-VM是走传统CS模型的C4框架,属于C4分支框架**)
- 起源来自一个很简单的需求,我用内网穿透给一个局域网开通了互联,这时候在不修改host不使用dns情况下,使用C4是无法穿透直连的,必须改用C4-VM框架
- C4需要匹配对等IP和端口,出现穿透以后无法访问,于是给C4做了这个补丁
- **该补丁是解决C4的偏门需求,当使用了vpn或则内网穿透这类技术以后,让C4仍然能保持一种对称+对等的网络环境**
- ZNet-C4网络拓扑结构补丁,不会影响现有项目群的正常工作,必须对整个c4网络中的全部节点重新编译后才可生效,单方面编译无用.
- **ZDB2新增高速空间申请机制,兼容操作系统可以支持各种阵列和虚拟化硬盘系统,良好支持NTFS,在win10以后系统可以支持到ReFS**

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
- 运行一个pb级的备份流程,时长可能大于24小时,根据设备的性质:全闪,全高速hdd,是否从设计层面分离了数据源和数据备份两套物理设备
- 近期忙于一个数据中心项目,数据量单日pb级,堆了n个hdd分组阵列,试运行第一天备份系统出现内存过载,直接宕机,第二天跟踪观察了一天,终于找到过载问题,修复!
- 加固底层BigList,OrderStruct的安全性,因为我在做大数据处理时根本不敢随意关闭服务器进程,我需要底层做到即使出现异常也要继续工作,做了一个简单加固
- 重做Z.Net内核库中的IOBusy功能,可以跟踪发送与接收数据中数据在汇集池的状态,返回的IO是否繁忙的状态会更准确
- 新增Last_IO_Is_IDLE数值,表示IO最后一次活跃的时间单位


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


**2023-1-6,新年开门红小更**
- 简单更新一些库命名
- 修复编译时error

**2022-11-27,大更新**

- **新增ZDB2高并发增删查改（单机）**
- **重大问题修复：新版本排序算法,彻底修复排序bug,全部测试通过**
- 提交了部分使用文档

**2022-10-30,大更新**

- **补充更新：新增高速配对算法Demo**
- **补充更新：安全阻塞**
- **补充更新：数据发送机制不再限制焦点主线程**,该更新对于安卓平台使用ZNet非常有用.**使用原则：仍以主线程为主**
- **解决android平台因为编译器造成的arc问题**,编译器兼容xe11.2,fpc3.2.x
- 重做流量均衡内核：底层大更,解决10W/s请求流量,造成服务器卡住的问题,该问题也类似cc攻击.该更新不影响现有服务群
- 重做底层hash系统：通讯能力将会出现1%小幅下降,兼容anroid的arc问题
- 新增一个C4 for android hello world的demo,IOS和OSX平台可同步
- 粘包机制大更新：不再使用copy方式切分数据,改用指针,可有效提升负载性能,同时也具有消化cc级请求的能力(其实大部分cc请求防火墙都可以拦截下来)
- 大家注意一下,ZNet已经被某些防火墙和操作系统平台加了数据跟踪线索,构建项目时尽量避免直接使用公共通讯参数,修改Z.Net.pas库中的部分命令和通讯参数可避免被跟踪
- 全部demo/tool编译测试通过,建议在线项目更新到该版本

**2022-10-9,应用项目修复**

- 实际应用项目中,发现许多问题,修复许多bug,改动非常多,无法一一叙述,核心机制无变化

**2022-6-19,置换设备后重启技术更新**

- hpc支持性大更新：分离出了物理网络数据和粘包流程,物理网络线程可处于无卡顿工作状态,解锁hpc高流量支持能力
- hpc支持性大更新：在无卡顿的物理网络机制中,由于缓冲技术需要使用内存,从而增加了一个磁盘暂存技术支持体系(使用ZDB2暂存)

```delphi
//hpc支持性使用说明,常量,直接修改Z.Net.pas文件

//BigStream每次发送的块大小,在万兆局域网中可以调大它,并发时它会很消耗内存,传大文件调大可提速
C_BigStream_Memory_SwapSpace_Activted: Boolean = False;
C_BigStream_SwapSpace_Trigger: Int64 = 1024 * 1024;

//在大量链接的服务器会用到主循环,该参数表示每次调用主循环的最大延迟,达到该延迟,循环会退出来,下次执行时会从检查点继续循环,默认100毫秒
//当服务器链接过多,且负荷不过来,该参能有效防止假死
TZNet.ProgressMaxDelay:TTimeTick

//启用或禁用BigStream磁盘缓存模型
C_BigStream_Memory_SwapSpace_Activted: Boolean = False;
//触发BigStream的缓存模型的条件,当发送队列的数据达到,就执行磁盘缓存（Stream必须是TMemoryStream,TMemoryStream64）
C_BigStream_SwapSpace_Trigger: Int64 = 1024 * 1024;

//启用或禁用CompleteBuffer磁盘缓存模型
C_CompleteBuffer_SwapSpace_Activted: Boolean = False;
//触发BigStream的缓存模型的条件,当发送队列的数据达到,就执行磁盘缓存,0表示全部缓存
C_CompleteBuffer_SwapSpace_Trigger: Int64 = 1024;

{ 在万兆网带宽峰值到来时,机器会处理不过来,这时候,缓存技术会把物理网络发来的数据记录到磁盘数据库,该常量是出发磁盘缓存的条件,当RX碎片达到100 }
C_Physics_Fragment_Cache_Activted: Boolean = False;
C_Physics_Fragment_Cache_Trigger: NativeInt = 100;
{ 缓存数据库,加密,步进尺寸 }
C_Swap_Space_Technology_Security_Model: Boolean = True;
C_Swap_Space_Technology_Delta: Int64 = 8 * 1024 * 1024;
C_Swap_Space_Technology_Block: Word = 1536;

//以上参数请自行摸索,hpc下的准万兆网支持模式还要考虑cpu调度,分线程,磁盘运用,数据库优化,多种因素决定结果
//并不是网络解决万兆项目就能跑出万兆效果

```

**2022-6-9,置换设备后重启技术更新**
```
针对hpc无法调试的修复更新: 在hpc使用cpuCount作为构建参数,程序启动就是300个线程,调试器会在启动时卡住
```

**2022-4-18针对ZDB2的革命性优化**

```
内核大更新,细节自行diff
优化Z.Net.pas库95%的链表数据结构,大规模负载时,内存消耗更小,而IO性能提升15%左右
ZDB2和C4相关服务,优化内核数据结构,针对大数据的增删插性能大约提升300倍
新增Hash泛型库
新增个demo：BigList vs List
ZDB2新增线程支持库
C4新增网盘VM框架
新增FS2 VM框架
新增TEKeyValue VM框架
新增Var VM框架
兼容Delphi XE7及以后版本,兼容FPC 3.x版本,支持FPC+ARM构建多平台
Tools, Adnvace demo, C4 Demo, Examples, 全部测试通过
```

**2022-4-8 兼容性更新**

```
感谢qq896616118,qq6192122,qq276678295, 提供无法编译信息,已修复兼容XE8及其以后版本,dcc+clang for OSX/ios/android未测试
```


**2022-4-6内核革命性大更**

```
本次大更全部集中于底层,对于上层应用群：大数据,AI,图像处理,网络,提供天然优化
重做线程池调度机制,重做Post Thread机制,重做TCompute线程触发机制,优化Z.Notify机制
Z.Core在内部新增一个支持64位地址段的泛型链表
重做MT19937随机数与线程实例匹配机制
DisposeObject调用不再会遍历互斥锁池,直接Free干掉
互斥锁池机制改为：UnlockObject(Obj)里面回收Critical句柄,释放内存
TCompute线程机制新增原子状态机变量：IsRuning,IsExit,完整兼容系统内核机制
synchronization机制（流程易错,效率超低）,以后用状态机轻松回避它
新增ZDB2线程支持内核库：Z.ZDB2.Thread.pas
```

**过去的更新日志**

- 2022-3-24,新的并网机制：不影响之前的DP入网机制,已测试通过.
- 2022-3-24,新增网盘支持核心,已测试通过.
- 2022-3-16,给ZDB1.0的数据引擎增加了cache测试单元,因为减懒不写test case,所以疏忽,导致产生深度性质的cache bug.
- 2022-3-16,修复ZDB1.0的底层Cache：这个bug太难fixed了
- 2022-3-13,机制更新：C4入网不会再产生无用的链接：例如Depend Network包含DP,而目标网络没有DP服务,则不会产生链接,此机制更新对现有应用群无影响
- 2022-3-13,console app给出了help接口：当console启动时可以通过help查阅服务器和隧道的当前状态,另：所有的demo均使用console app模式
- 2022-3-13,修复DocUserDBDemo的启动问题
- 2022-3-13,优化DP接口：不再生成无用连接,即使生成链接,在默认5秒以后,也会kill掉无用的连接
- 2022-3-13,下个版本的ZNet将会支持网盘,更新幅度应该较大（大家放心,C4不会改机制,不影响现有应用群）,本次更新的所有demo和库都测试通过,属于稳定版本（已经提交Release包）

3-13 机制更新说明: **GetOrCreatePhysicsTunnel/SearchServiceAndBuildConnection**
```pascal
// GetOrCreatePhysicsTunnel 入网以后不用再管了,有新服务器入网时它自动调度,离线也是自动处理
// 该方法需要保证C4网络有DP
C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel('127.0.0.1', 8888, 'dp|myService', nil);

// SearchServiceAndBuildConnection优于GetOrCreatePhysicsTunnel
// SearchServiceAndBuildConnection 是一次入网,不需要DP,不支持运行时调度
// 该方法只要C4网络有服务器标识符,都能入网
// SearchServiceAndBuildConnection 可以支持最优负载
C40_PhysicsTunnelPool.SearchServiceAndBuildConnection('127.0.0.1', 8888, 'myService', nil);

// 如果要支持运行时调度,只需要包含DP
C40_PhysicsTunnelPool.SearchServiceAndBuildConnection('127.0.0.1', 8888, 'DP|myService', nil);
```
3-13更新C4入网的命令行脚本: **AutoTunnel/Tunnel**
```
// C4脚本系统中的入网脚本,AutoTunnel方式优于Tunnel
// autoTunnel=SearchServiceAndBuildConnection方式入网,为一次性
// AutoTunnel(地址,端口,标识符,从服务器群选择负载最小的为false表示连上标识符为myService的全部服务器)
myClient "AutoTunnel('127.0.0.1',9188,'DP|myService',False)"

// Tunnel=GetOrCreatePhysicsTunnel方式入网,为一次性
// Tunnel(地址,端口,标识符最好带有DP,这样C4才可以有自动化机制)
myClient "Tunnel('127.0.0.1','9188','DP|myService')"
```

- 2022-2-13,修复了一个ZDB2重大bug：flush cache一个变量疏忽,导致写入错误的问题
- 2022-2-13,在AdvanceDemo中新增大文件备份演示
- 2022-2-10,qq375960048田华提了一个问题,dp在查询以后,客户端默认5秒才会断开服务器连接,这时候可能出现socket close信号无法到达服务器,导致客户端断开以后服务器连接仍然存在,今天主要修复这个问题：对服务器来说,凡事连接但p2pVM未创建都按5秒强断机制处理.
- 2022-2-10,近期qq345148965阿木/田华/qq860457224老廖,他们,正在应用ZNet,我或许会对他那边的需求做一些定制化的更新,大家尽请期待.

**文档篇幅有限,项目挺忙,暂时就写这么多,空了再为大家继续完善文档**
- by.qq600585
- 2022-1-19

