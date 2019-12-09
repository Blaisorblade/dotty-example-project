// package bug
// import reflect.Selectable.reflectiveSelectable

// object options2 {
//   type Option[T] = {
//     def isEmpty: Boolean
//   }
//   type None[T] = Option[T]
//   //Succeeds:
//   val none : () => Option[Nothing] = () => new {
//     def isEmpty = true
//   }
//   //Fails. Note I'm not even using T!
//   val mkNone0 : [T] => () => Option[Nothing] = [T] => () => new {
//     def isEmpty = true
//   }
//   //Fails. That'd be real.
//   val mkNone : [T] => () => Option[T] = [T] => () => new {
//     def isEmpty = true
//   }
// }
