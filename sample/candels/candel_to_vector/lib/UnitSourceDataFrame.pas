unit UnitSourceDataFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Grid,
  FMX.ScrollBox,
  FMX.Controls.Presentation, FMX.Layouts;

type
  TSourceDataFrame = class(TFrame)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Label1: TLabel;
    Label2: TLabel;
    StringGridSource: TStringGrid;
    CandelColumnID: TStringColumn;
    CandelColumnDate: TStringColumn;
    CandelColumnTime: TStringColumn;
    CandelColumnOpen: TStringColumn;
    CandelColumnHigh: TStringColumn;
    CandelColumnLow: TStringColumn;
    CandelColumnClose: TStringColumn;
    CandelColumnVol: TStringColumn;
    StringGridVector: TStringGrid;
    VectorColumnID: TStringColumn;
    VectorColumnDate: TStringColumn;
    VectorColumnTime: TStringColumn;
    VectorColumnOpen: TStringColumn;
    VectorColumnHigh: TStringColumn;
    VectorColumnLow: TStringColumn;
    VectorColumnClose: TStringColumn;
    VectorColumnVol: TStringColumn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
