Prelude.Debug.show
  { unsafePartial: 
      Prelude.Partial.unsafePartial
        case true of
          true -> true
  , fromPartial:
      Prelude.Partial.fromPartial true
        case true of
          false -> false
  }