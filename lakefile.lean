import Lake
open Lake DSL

package «forLoopSpeed» {
  -- add package configuration options here
}

lean_lib «ForLoopSpeed» {
  moreLinkArgs := #["-O3", "-UNDEBUG"]
  -- add library configuration options here
}

@[default_target]
lean_exe «forLoopSpeed» {
  buildType := .release
  moreLinkArgs := #["-O3", "-UNDEBUG"]
  root := `Main
}

