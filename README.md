## ZNet是ZServer4D的技术重构和母体转移

#### 说说起源

ZServer4D在长期更新积累下，因为需要兼容许多项目群，导致了项目内部各种定义，命名，机制很难统一，在应用通讯技术到新领域时，这些不利因素都是障碍。

在2022年1月，我用机器化模型技术，对庞大的项目群，以及ZServer4D做了一次升级，简单来说，就是创造代码机器，然后大规模升级ZServer4D和项目群。目前，我已经完成了所有的编译和测试工作。

## ZNet的技术体系
#### 说说ZNet定位

ZNet在以后的主要定位是对SaaS，p2pVM，IOT，大数据存储IO（仅仅是IO），现役项目群，提供通讯地基支持。大家可以选择使用Pascal Rewrite Model技术直接把项目升级到ZNet，可以把ZNet直接重构成ZServer4D继续使用。

**ZNet是未来的ZServer4D母体，母体就是发源地，以后创造起源都会基于ZNet进行更新**，Pascal Rewrite Model技术可以让我们随时把最新的ZNet转换成ZServer4D或则别的分支项目，母体则是ZNet。


## 把ZServer4D项目群升级到ZNet
#### 主要使用代码重写技术：Pascal Rewrite Model，全自动化升级

- 代码重写模型简单来说，就是用机器代替了人类去规范化代码的技术，而规范化其实也就是人性化，更人性化表示更容易被长期提供支持和维护，因为pascal基本上都是35岁向上，工作的时间代价非常高，必须重视重复劳动
- 一个技术问题：**Pascal的编译机制决定了它Unit命名非常容易重复**，当三方Unit库文件达到1000数量级会高几率出现重复命名，Pascal Rewrite Model技术可以很好解决大规模并库问题
- Pascal Rewrite Model技术并不是ZNet专用，在我看来它可以辅助我们把项目优美的堆起来
- Pascal Rewrite Model技术可以正反双向重写代码：ZNet可以直接转换成ZServer4D，ZServer4D也可以直接升级到ZNet，其它项目依次类推
- **我会开源Pascal Rewrite Model技术，我的目标是帮助大家更好的做创造和创新，而不是借用新技术收割大家**
- 关于引导使用Pascal Rewrite Model，包括制作模型，应用模型，我已彻底开放这一技术体系，让每个人都能做到每天在几十万处diff中自由穿梭，不会局限于ZNet和ZServer4D互转

2022-1-19


