## ZNet is a technological restructuring and matrix transfer of ZServer4D


### Purpose of ZNet


In the field of servers, communication functions have become very popular, and at this time, the focus has become more focused on server capabilities. The ZNet system stands from the perspective of computer mechanisms, with structures and algorithms as auxiliary, from the ZDB2 big data system+modern generic structure system+advanced thread control system, to comprehensively improve the framework of server work capabilities


**For ZNet, the communication system is just an interface, which is not important. The operation inside the server is the key**. For example:


- The data processing center of the web uses ZNet to support modern project systems through web APIs
- Integrate communication systems within IOT and AIOT
- Entrepreneurial projects involve a lot of communication and there are no resources around the world. If you start DIY projects yourself, you can try ZNet
- Exceeding conventional big data storage and analysis projects, with data volume reaching PB level
- Small communication tools, such as file upload and download management, intranet penetration, and automated deployment
- Z-AI's 6th generation monitoring data center backend uses ZNet to solve the problem

Before paying attention to and using ZNet, it is necessary to do a lot of programming and delve into data structures to prepare for business implementation from a computer mechanism perspective


#### Talking about the Origin


Under the long-term accumulation of updates, ZServer4D needs to be compatible with many project groups, which makes it difficult to unify various definitions, naming, and mechanisms within the project. These unfavorable factors are obstacles when applying communication technology to new fields

In January 2022, I used mechanized modeling technology to upgrade a large project group and ZServer4D. Simply put, I created a code machine and then upgraded ZServer4D and the project group on a large scale. Currently, I have completed all the compilation and testing work

## Technical positioning of ZNet

- ZNet believes that programming should follow computer science. ZNet does not play with concepts and designs, and all problems are solved using the simplest methods: algorithm+mechanism+API=solve
- The original intention of ZNet design was to focus on programming and provide C4 for large-scale projects
- ZNet is frequently updated, mostly with new features and minimal fixed bug frequency
- No technical gimmicks, all solution information on ZNet will correspond to the project landing

The main positioning of ZNet is to provide communication foundation support for SaaS, p2pVM, IOT, big data storage IO, active project group

Please refer to the reference manual for details https://zpascal.net/ZNetManual.pdf

#### Major update of ZNet compared to ZServer4D

- Truly supports n Wan Zao network card payloads, with daily throughput reaching petabytes
- Built in modern number structure and algorithm support system, including thread system support across NUMA, coroutine system support, exclusive structured linked list system with over 10 times faster speed compared to TList, structure matcher, and structure hash container system
- ZDB2+ZNet has a complete support system for idc computer room level projects
- C4 SaaS system support
- The built-in third-party libraries in ZNet are all exclusive versions after comprehensive refactoring and fixed
- Deployment support for infrastructure services provided by IDC
- The communication kernel fully supports multithreading, can simulate blocking on its own, and encapsulate communication protocols
- Compared to ZServer4D, the high traffic processing capacity is increased by 20 times and upward
- Communication programs built with fpc can also have ultra-high traffic and large server architecture capabilities on the IOT platform

## Target architecture executable testing and network driven

- Dcc=The dcc compiler supports this third-party communication library
- Fpc=The fpc compiler supports this third-party communication library
- N/a=The dcc/fpc compiler cannot support this third-party communication library

| test   | Cross | synapse | indy | ICS | ICSV9 | DIOCP |
| ------- | :------: | :------: | :------: | :------: | :------: | :------: |
| x86  | dcc | dcc | dcc | dcc | dcc | dcc |
| x64  | dcc | dcc | dcc | dcc | dcc | dcc |
| ARM  | fpc | fpc | fpc | n/a | n/a | n/a |
| MIPS  | fpc | fpc | fpc | n/a | n/a | n/a |


## Recommendations for communication interfaces for multiple systems

- RS=The server supports this platform and is recommended to use it
- RC=The client supports the platform and recommends adopting it
- CR=Supports the platform but only serves as a candidate recommendation
- CC=Supports the platform but only serves as a candidate recommendation
- NR=The server does not support this platform
- NC=The client does not support this platform

| test   | Cross | synapse | indy | ICS | ICSV9 | DIOCP |
| ------- | :------: | :------: | :------: | :------: | :------: | :------: |
| linux(FPC)  | RS/CC | CR/RC | NR/NC | NR/NC | NR/NC | NR/NC |
| win7  | RS/RC | CR/CC | CR/CC | CR/CC| CR/CC | CR/CC |
| win10  | RS/RC | CR/CC | CR/CC | CR/CC| CR/CC | CR/CC |
| win2019  | RS/RC | CR/CC | CR/CC | CR/CC| CR/CC | CR/CC |
| win2022  | RS/RC | CR/CC | CR/CC | CR/CC| CR/CC | CR/CC |
| CN-Linux(FPC)  | CR/CC | RS/RC | NR/NC | NR/NC | NR/NC | NR/NC |
| Android  | RS/RC | NR/NC | CR/CC | NR/NC | NR/NC | NR/NC |
| OSX/IOS  | NR/NC | NR/NC | RS/RC | NR/NC | NR/NC | NR/NC |

### About Project Stacking

Many open source projects seem simple and easy to understand, but in real server projects, the scale may be 100 times larger than open source projects. From quantitative to qualitative changes, increasing the scale by 100 times is no longer a conventional technical solution

Many people use a scheduling database and communication system for servers. If they are working on the web, the server will also include a design UI system. If the project size is very small: communication frequency is low and communication data volume is small, there is no problem with how to operate this type of server

When the server size starts to be larger: communication frequency is high and communication data volume is large, it will face the challenge of programming around hardware

The hardware positioning of 6-core 12 hyperlines and 128-core 256 hyperlines is different, as it affects the design models of threads and servers. 6-core hardware can hardly generate parallel program models, even if one link is running parallel for, the entire server may be affected. 6-core hardware can only generate regular threads to load particularly heavy computing links. When the server runs on a 128-core platform, there is no problem with CPU computing resources, Instead, it is about arranging computing resources reasonably, such as 4-thread parallel computing, which can sometimes be faster than using 40 threads. Finally, there is a system bottleneck. Without the use of third-party thread support libraries, a single process in the Win system can only use up to 64 hyperlines at the same time. Only after TBB and other libraries are mounted can 256 hyperlines, full threads, partial threads, and single threads be used simultaneously, The positioning of this type of server is completely different from the beginning of its design. The center of the server program is around the hardware trends of different eras. The biggest reason is that the hardware platform and framework design are small steps. Only when the hardware is clearly defined as the center, can server design and programming be done, which is the correct path

For example, 8*4T full flash sdds with 192GB of memory, and 8 16T hdds with 1TB of memory. These servers also have different designs for data storage and caching systems. For example, when the data size reaches 3 billion pieces and the space consumption reaches 30T, 192GB of memory is basically very tight, but it is not a problem. After careful optimization, they can still run because caching is always used to speed up data search or storage, When the hardware configuration of the storage device uses hdd and the capacity reaches 100T, with 1TB of memory, this design will require more cache optimization than 192G. After the traffic enters, it is possible that just write cache will directly consume 0.99TB of memory. The program has already positioned 10G for hash indexing and developed various optimization algorithm structures at the beginning of the design. These two different hardware configurations are different at the system design level of the server: 192G only needs to consider optimizing the cache size, 1TB requires consideration of optimizing cache size and preventing crashes, as hdd writing big data is very prone to crashes when the traffic exceeds the array write limit. Once the memory of the large array is used up, the IO capacity of the array may decrease to 5% of its original capacity, which is no different from crashes. Cache control (flush) will be one of the core mechanisms directly brought to the front end, which requires overall control of key elements such as big data input, array write mechanism, and hardware locking

Finally, there is the GPU. Once a server encounters a GPU, the supporting devices from CPU to storage will almost always be highly configured. At this time, the server will completely break away from the classical single threaded approach in programming, and the process model will be replaced by the pipeline model. These pipelines will flow from one operating system to another. At this time, the ZNet program will all follow the route of Buffer+threads, and this model only connects data in, The computing entity is a collection of independent and massive computing support systems

Please refer to the reference manual for details https://zpascal.net/ZNetManual.pdf

### Upgrade the ZServer4D project group to ZNet

**Mainly using code rewriting technology: Pascal Rewrite Model, fully automated upgrade**

- In simple terms, the code rewriting model refers to the technology of using machines to replace humans to standardize code. Normalization is actually humanization, which means it is easier to be supported and maintained for a long time. Because Pascal is basically 35 years old and the time cost of work is very high, it is necessary to pay attention to repetitive labor
- A technical issue: * * Pascal's compilation mechanism determines that its Unit naming is very easy to duplicate * *. When third-party Unit library files reach the order of 1000, there is a high probability of duplicate naming. Pascal Rewrite Model technology can effectively solve large-scale library merging problems
- Pascal Rewrite Model technology is not exclusive to ZNet, in my opinion, it can help us stack projects gracefully
- Pascal Rewrite Model technology can rewrite code in both positive and negative directions: ZNet can be directly converted to ZServer4D, ZServer4D can also be directly upgraded to ZNet, and other projects can follow suit
- **I will open source Pascal Rewrite Model technology, and my goal is to help everyone create and innovate better, rather than borrowing new technologies to harvest everyone**
- Regarding guiding the use of Pascal Rewrite Model, including creating and applying models, I have completely opened up this technical system, allowing everyone to freely shuttle through hundreds of thousands of diffs every day, without being limited to the mutual conversion between ZNet and ZServer4D

**Upgrade operation method**

```
In the https://github.com/PassByYou888/ZNet/releases Find the PRP program
Upgrade all delphi/fpc projects and code in the c: myproj directory to use the Znet library, including. pas. inc. dfm. fmx. dpr. lpr
Command line format prp.exe "- D: c: myproj "
The PRP program defaults to turning on parallel mode. If the machine configuration is not sufficient to turn off parallel, the model will take longer to process the code
Command line format prp.exe - P: OFF "- D: c: myproj "
After upgrading, if you find yourself not accustomed to or unwilling to use ZNet, you can choose to downgrade the project and code of c: myproj to ZServer4D
Command line format prp.exe -R "-D:c:\myproj"
Description - R switch represents reverse rewrite

By analogy, we can also use prp to directly downgrade ZNet to ZServer4D and then overwrite ZServer4D in the copy process
Command line format prp.exe -R "-D:c:\ZNet"
```


**Compile PRP program**

```
Set the compilation path for ZNet first
The PRP program is located in ZNet Tools PascalUnitRewriteTool
Simply open prp.dproj to compile it
PascalRewriteModel.dproj is a modeling tool for PRP that can be compiled. However, this document does not provide modeling guidelines at this time. Please explore it yourself for now
In the future, I will create a special project to explain the modeling method of the PascalRewrite model
```

by.qq600585
