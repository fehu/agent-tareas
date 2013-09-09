package feh.tec.visual.helpers

import java.awt.Color

trait ColorHelpers {
  implicit class ColorWrapper(color: Color){
    def toIntRgb = (color.getRed, color.getGreen, color.getBlue)
    def toIntRgba = (color.getRed, color.getGreen, color.getBlue, color.getAlpha)

    def toFloatRgb = (color.getRed.toFloat, color.getGreen.toFloat, color.getBlue.toFloat)
    def toFloatRgba = (color.getRed.toFloat, color.getGreen.toFloat, color.getBlue.toFloat, color.getAlpha.toFloat)
  }
}

object ColorHelpers extends ColorHelpers