import Lake
open Lake DSL

package "cdown" where
  -- add package configuration options here

lean_lib «Cdown» where
  -- add library configuration options here

@[default_target]
lean_exe "cdown" where
  root := `countdown
