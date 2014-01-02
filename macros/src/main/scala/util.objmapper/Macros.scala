package util.objmapper


import language.experimental.macros

object Macros {

  def objMapper[TFrom, TTo](): ObjMapper[TFrom, TTo] = macro MacrosImpl.objMapperImpl[TFrom, TTo]

}
