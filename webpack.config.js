const path = require('path');
const webpack = require('webpack');
const WebpackNotifierPlugin = require('webpack-notifier');

const config = {
  entry: './src/index.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'bundle.js'
  },
  devServer: {
  	contentBase: "./dist",
    hot: true
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/, /Stylesheets\.elm$/],
        use: [
          'elm-hot-loader',
          'elm-webpack-loader',
        ]
      },
      {
        test: /Stylesheets\.elm$/,
        use: [
          'style-loader',
          'css-loader',
          "postcss-loader",
          'elm-css-webpack-loader'
        ]
      },
      {
        test: /\.css$/,
        use: [
          'style-loader',
          'css-loader',
          "postcss-loader"
        ]
      }
    ]
  },
  plugins: [
    new webpack.NamedModulesPlugin(),
    new webpack.HotModuleReplacementPlugin(),
    new WebpackNotifierPlugin(),
  ]
};

module.exports = config; 
