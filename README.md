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

**更新日志**

- 2022-2-10，qq375960048田华提了一个问题，dp在查询以后，客户端默认5秒才会断开服务器连接，这时候可能出现socket close信号无法到达服务器，导致客户端断开以后服务器连接仍然存在，今天主要修复这个问题：对服务器来说，凡事连接但p2pVM未创建都按5秒强断机制处理。
- 2022-2-10，近期qq345148965阿木/田华/qq860457224老廖，他们，正在应用ZNet，我或许会对他那边的需求做一些定制化的更新，大家尽请期待。

**文档篇幅有限，项目挺忙，暂时就写这么多，空了再为大家继续完善文档**
- by.qq600585
- 2022-1-19


