package util.objmapper


trait ObjMapper[TFrom, TTo] {
  def mapValue(obj: TFrom): TTo
}
object ObjMapper {
  /**
   * Maps a value from the type TFrom to TTo
   */
  @inline
  def mapValue[TFrom, TTo](obj: TFrom)(implicit mapper: ObjMapper[TFrom, TTo]) :TTo = {
    mapper.mapValue(obj)
  }
}
