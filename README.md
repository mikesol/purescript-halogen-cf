# purescript-halogen-cf

A library for writing comonadic renderers with Halogen.

## Structure

The library is comprised of two modules:

- `Halogen.Cf`: core library.
- `Halogen.Cf.Sugar`: one-liners for stuff that I found myself writing over and over.

## Basic example

```purescript
myComponent = HCf.component HCf.defaultOptions
  ({ count: 0 } # fixCf \render { count } _ -> do

    pure $
      HH.button
        [ HE.onClick \_ -> HCf.doThis render ({ count: count + 1 }) ]
        [ HH.text $ show count ])
```

## Example

Check out the [./example](./example). A live version is [here](https://purescript-halogen-cf.surge.sh/).

## Contributing

Contributions are welcome! If you feel anything could be improved or needs more clarity, please don't hesitate to open an issue or make a pull request.

## Goals

- Performance
- Type-safety
- Small core API (less than 300 lines of code)
- Small components
