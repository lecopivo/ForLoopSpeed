import Lake
open Lake DSL

package «forLoopSpeed» {
}

lean_lib «ForLoopSpeed» {
}

@[default_target]
lean_exe «forLoopSpeed» {
  buildType := .release
  root := `Main
}

