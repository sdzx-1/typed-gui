# 尝试将typed-fsm与GUI结合，产生了意想不到的抽象组合

本文需要你对typed-fsm有所了解，如果你不了解typed-fsm，可以看看我写的介绍typed-fsm的文章。

# 核心想法
让我们以一个TodoList的gui项目为例：
![origin](data/origin.png)

这里的每个状态都代表了一个页面：

Main 代表显示todolist内容主页面

AreYouSure 代表选择Yes或者No的页面

Modify 代表修改某一条todo的页面

Add 表示添加一条todo的页面·

Exit 代表退出

每一个箭头代表一个消息

### 整体的运行框架如下：
![framewrok](data/framework.png)

UI先渲染界面注册事件，FSM Handler等待事件传入，然后处理事件。然后如此循环即可。

整体结构就是如此简单！

UI 部分使用的是修改后的[threepenny-gui](https://github.com/sdzx-1/threepenny-gui), 主要的改动是将原来的(UI a)修改为(UI ps (t::ps) a)。这样做的是为让on注册的事件中包含状态机目前的状态，这样能保证当UI界面触发事件时，向状态机发送正确的消息。
```haskell
on :: (element -> Event a) -> element -> (a -> UI ps t void) -> UI ps t ()
on f x = void . onEvent (f x)
```

OK，核心的想法已经解释完毕。下面我将解释在todoList这个具体例子中产生的意想不到的抽象组合
# 意想不到的抽象组合

让我们回到这张图片：
![origin](data/origin.png)
可以发现AreYouSure出现了多次，并且Modify，Add的工作模式相同。
于是我们可以将这两个模式提取出来。
![two](data/two.png)
注意这里在Action中甚至复用了AreYouSure的状态。

对于Action甚至有通用的处理函数
```haskell
actionHandler'
  :: forall action to
   . (SingI to, SingI action)
  => Op Todo (AllState Todo TodoList) IO (Maybe (ActionOutput action)) to (Action action to)
actionHandler' =
  getInput I.>>= \case
    SureAction val ->
      getInput I.>>= \case
        Yes -> returnAt (Just val)
        No -> I.do
          liftm $ putSt @action (sing @action) (InternalSt $ Right val)
          actionHandler'
    ExitAction -> returnAt Nothing

```

于是我们新的设计如下：
![new](data/new.png)
这真是巨大的简化！！！

状态机的定义如下：
```haskell
$( singletons
    [d|
      data Todo
        = Main
        | Add
        | Delete
        | Modify
        | Exit
        | Action Todo Todo
        | AreYouSure Todo Todo
        deriving (Show, Eq, Ord)
      |]
 )

```

状态转移消息如下：
```haskell
instance StateTransMsg Todo where
  data Msg Todo form to where
    Yes :: Msg Todo (AreYouSure from to) to
    No :: Msg Todo (AreYouSure from to) from
    ------------
    ExitAction :: Msg Todo (Action action from) from
    SureAction
      :: ActionOutput action
      -> Msg
          Todo
          (Action action from)
          (AreYouSure (Action action from) from)
    --------------
    EnterAdd
      :: ActionInput Add
      -> Msg Todo Main (Action Add Main)
    EnterModify
      :: ActionInput Modify
      -> Msg Todo Main (Action Modify Main)
    DeleteOne
      :: Int
      -> Msg Todo Main (AreYouSure Main Main)
    -----------------
    IsExitTodo :: Msg Todo Main (AreYouSure Main Exit)
    ExitTodo :: Msg Todo Main Exit
```
# 总结
[代码在这里](https://github.com/sdzx-1/typed-gui/tree/demo)

一个简单的todoList的例子，似乎让我看到了将typed-fsm与GUI结合在一起的巨大潜力。

这种意想不到的组合让我非常惊讶！

有人对基于这种方式构建GUI感兴趣的吗？
