# 程序运行方法

### 1. 核心语言、代数数据类型

由于可以采取黑盒测试，因此没有显式的执行方法，主要体现在测试的调用。

### 2. Parser

进入项目文件夹后执行下面的语句

```bash
stack run
```

进入程序后输入`parser`按回车，再输入想要测试的语句后，按下回车后，下面会显示解析的AST，表达式的类型，以及表达式的结果。

### 3. REPL

进入项目文件夹后执行下面的语句

```bash
stack run
```

进入程序后输入`repl`按回车，就进入了交互式环境，具体的使用方法请参照实验报告中的实现目标。