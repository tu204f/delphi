object GridFrame: TGridFrame
  Left = 0
  Top = 0
  Width = 364
  Height = 301
  TabOrder = 0
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 364
    Height = 301
    Align = alClient
    Color = clBtnFace
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Height = 27
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Images = ImageList
    LineMode = lmBands
    StateImages = ImageList
    TabOrder = 0
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme, toHideTreeLinesIfThemed]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnBeforeCellPaint = VSTBeforeCellPaint
    OnChange = VSTChange
    OnClick = VSTClick
    OnGetText = VSTGetText
    OnPaintText = VSTPaintText
    OnGetImageIndex = VSTGetImageIndex
    OnHeaderClick = VSTHeaderClick
    Columns = <>
  end
  object ImageList: TImageList
    Left = 128
    Top = 144
  end
end
