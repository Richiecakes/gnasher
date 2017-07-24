package gnasher.reactive.parsing

import gnasher.reactive.model._

private[parsing] object Checker {

  private def checkArena(arena: ArenaAST): Arena = {
    val arenaVarList = arena.vars
    val arenaVars  = arenaVarList.toSet
    if (arenaVarList.size != arenaVars.size) err("Arena variables are not unique.")
    val modules = arena.modules.map(m => checkModule(m, arenaVars))
    Arena(arena.name, arenaVars, Modules(modules))
  }

  private def checkAssignments(assignments: Seq[AssignAST]): Seq[Assign] = {
    assignments.map(assignAst => Assign(assignAst.varName, assignAst.formula))
  }

  private def checkGuards(inits: Seq[GuardAST], moduleName: String, moduleVars: Set[String]): Seq[Guard] = {
    for {
      init <- inits
      assignment <- init.assignments
    } {
      if (!moduleVars.contains(assignment.varName))
        err(s"Module $moduleName contains bad assignment to ${assignment.varName}. (Not in ${moduleVars.mkString(",")})")

    }
    inits.map(guardAst => {
      val assignments = checkAssignments(guardAst.assignments)
      Guard(guardAst.condition, assignments, moduleVars)
    })
  }

  private def checkModule(module: ModuleAST, arenaVars: Set[String]): Module = {
    val moduleVarList = module.vars
    val moduleVars = moduleVarList.toSet
    if (moduleVarList.size != moduleVars.size) err("Module variables are not unique.")
    if (!moduleVars.subsetOf(arenaVars))
      err(s"Variables ${(moduleVars -- arenaVars).mkString(",")} in module ${module.name} are not listed in the arena variables.")
    val inits = checkGuards(module.inits, module.name, moduleVars)
    val updates = checkGuards(module.updates, module.name, moduleVars)
    Module(module.name, moduleVars, inits, updates)
  }

  private def err(msg: String) = throw ReactiveCheckerException(msg)

  def apply(ast: ReactiveAST): Arena = ast match {
    case arena: ArenaAST => checkArena(arena)
    case _ => err("Given syntax tree was not a complete arena.")
  }
}
