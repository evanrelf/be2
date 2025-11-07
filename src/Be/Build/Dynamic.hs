module Be.Build.Dynamic
  ( BuildState'
  , TaskState'
  )
where

import Be.Build.Static qualified as Static
import Be.Value (SomeValue)

type BuildState' = Static.BuildState SomeValue SomeValue

type TaskState' = Static.TaskState SomeValue SomeValue
