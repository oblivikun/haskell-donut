# Haskell Donut

This program visualizes a donut in a 3D space and animates it in a console application. It uses the `Data.Vector`, `System.Console.Terminal.Size`, `Data.Bifunctor`, `Data.Ix`, and `Data.List.Split` libraries.

## Features

- **Creation of Circle Points**: Defines a function `createCirclePoints` that creates a list of points on a circle in 3D space.
- **Creation of Torus Points**: Provides a function `createTorusPoints` that takes two angles and returns a list of points on a torus.
- **Calculate Brightness**: Contains a function `calculateBrightness` that calculates the brightness of a point on a shape.
- **Convert Brightness to Character**: Includes a function `convertBrightnessToCharacter` that converts a brightness value to a character.
- **Sort By Depth**: Offers a function `sortByDepth` that sorts points by depth.
- **Transform 3D to 2D**: Contains a function `transform3Dto2D` that transforms a 3D vector to a 2D position.
- **Arrange Points in Grid**: Provides a function `arrangePointsInGrid` that arranges points in a grid.
- **Display Torus**: Includes a function `displayTorus` that displays a torus given two angles.
- **Animate Torus**: Offers a function `animateTorus` that animates a torus given two angles.
- **Main Function**: Contains the main function `main` that gets the terminal size and starts the animation of the torus.


## Dependencies

This module on the following libraries:

- `Data.Vector`
- `System.Console.Terminal.Size`
- `Data.Bifunctor`
- `Data.Ix`
- `Data.List.Split`

Make sure these libraries are installed in your Haskell environment before using this.

## License

This module is open source and licensed under the GPL License. See the LICENSE file for details.
