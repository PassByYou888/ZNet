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
ZDB2+ZNet是idc机房级项目的完整支持体系,大数据+稳定服务器=稳定后台
新增C4-SaaS系统的XNAT支持
新增兼容fpc/delphi的文本分析技术
开始全面走代码规范化路线 diocp ics synapse 这些库 代码模型全部加了Z前缀 以后使用ZNet不再会与这些库发生冲突
提供大数据支持地基
针对IDC提供的基建服务部署C4，例如我们的服务器主机不在主干网 C4网络可以绑定一个ip 再侦听另一个IP
发送机制不再限制运行焦点必须主线程
客户端具备安全阻塞机制
高流量处理能力相比ZServer4D提升5倍向上
```

### ZNet是地基型项目

- ZNet认为编程就应该遵重计算机理,ZNet不玩框架,一切问题都用最朴实的办法解决:算法+机理+API=solve
- ZNet设计初衷就是注重编程,并且提供了堆大项目的C4框架
- ZNet更新很频繁,这代表技术一直在进步
- 商业项目使用ZNet不会被绑架,内部自带很多结构+线程化支持地基

### 关于多平台

- ZNet本身可以兼容全部平台,包括最近刚推出的国产化平台
- ZNet在FPC+Lazarus构建项目时,默认客户端为阻塞方式
- ZNet在Delphi构建项目时默认兼容delphi支持的全部平台

### 关于项目堆大

- 项目堆大时建议直接引用C4, 因为C4框架就是为了解决堆大问题
- 其次是C4设计服务器完全基于p2pVM虚拟化框架,在跨平台移植,和10年后的兼容均会远高于控件和库依赖
- **C4框架有许多库,这些库都代表某个项目时期的服务器组件,这些组件并不重要,并且未来C4的组件会不断的增多,因为C4的开发模式就是堆组件**
- 建议单服务行数不要超过3000,否则难以维护,如超过可以分离服务器与客户端,如果再次超过,可以1分2

### 关于项目运营

- 不要尝试remote debug之类的功能,运营期间服务器的启停代价很大
- 把需要监控的服务器状态内容,使用Console debug做成命令行函数,如果服务器出错,用运行状态来分析

### dcc与fpc兼容性

- DCC兼容XE10.x,测试通过包括10.1
- DCC兼容XE11.x,测试通过包括11.3
- FPC兼容3.2.2,测试通过包括Lazarus-2.2.6

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

##最新更新日志

**2023-9-11 解决:在IO读写分离以后,写入变慢,当ZDB2跑阵列系统将带宽不够**

- ZDB2有一个空间分配表,当ZDB2的单库达到100TB极限时,单库空间表会逼近2GB,这时候,100个库使用flush方法回写空间表,将会出些200GB的瞬间写入流量
- 当flush按每5秒持续回写,将会卡住异步线程中的command-list请求队列
- ZDB2的解决入手方法1:只对发生了修改的分配表做回写操作,先做初步的动态表判断,如果满足条件,把空间表切割出来,只写入修改的数据,最终阵列带宽降低非常明显!
- ZDB2的解决入手方法2:在IO读写分离结构中,给出了新的优化跨度算法,这是一种范围型的hash方法,表示哪些范围被写入过数据,缓冲底层IO请求,例如,1个小时不做flush操作,内存中留有1000万个写数据,高速复写!


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

