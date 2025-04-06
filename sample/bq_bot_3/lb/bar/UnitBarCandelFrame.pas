unit UnitBarCandelFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects;

type
  TBarFrame = class(TFrame)
    RectangleBar: TRectangle;
    GridLayout: TGridPanelLayout;
    LayoutInfo: TLayout;
    LayoutBar: TLayout;
    LinePriceHigh: TLine;
    LinePriceLow: TLine;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
