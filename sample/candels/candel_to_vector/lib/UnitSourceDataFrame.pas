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
  FMX.Controls.Presentation,
  FMX.Layouts,
  FMX.Objects,
  Lb.Candel.Blocks;

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
    Rectangle: TRectangle;
    CandelColumnStatus: TStringColumn;
    VectorColumnStatus: TStringColumn;
  private
    FBlock: TBlock;
    function GetBlock: TBlock;
    procedure SetBlock(const Value: TBlock);
  public
    procedure SetShowBlock(ACountSource, ACountVector: Integer; const IsCandel: Boolean = True);
    property Block: TBlock read GetBlock write SetBlock;
  end;

implementation

{$R *.fmx}

uses
  Lb.Candel.SysUtils;

{ TSourceDataFrame }

function TSourceDataFrame.GetBlock: TBlock;
begin
  Result := FBlock;
end;

procedure TSourceDataFrame.SetBlock(const Value: TBlock);
begin
  FBlock := Value;
end;

procedure TSourceDataFrame.SetShowBlock(ACountSource, ACountVector: Integer; const IsCandel: Boolean);

  procedure SetGrid(AGrid: TStringGrid; ARow: Integer; ACandel: TCandel);
  begin
    AGrid.Cells[0,ARow] := IntToStr(ARow + 1);
    AGrid.Cells[1,ARow] := DateToStr(ACandel.Date);
    AGrid.Cells[2,ARow] := TimeToStr(ACandel.Time);
    AGrid.Cells[3,ARow] := FloatToStr(ACandel.Open);
    AGrid.Cells[4,ARow] := FloatToStr(ACandel.High);
    AGrid.Cells[5,ARow] := FloatToStr(ACandel.Low);
    AGrid.Cells[6,ARow] := FloatToStr(ACandel.Close);
    AGrid.Cells[7,ARow] := FloatToStr(ACandel.Vol);
    AGrid.Cells[8,ARow] := IntToStr(ACandel.Status);
  end;

var
  xCandel: TCandel;
  i, iCount, xRow: Integer;
begin

  StringGridSource.RowCount := ACountSource + 1;
  StringGridVector.RowCount := ACountVector;
  xRow := 0;
  iCount := FBlock.Sources.Candels.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      if IsCandel then
        xCandel := FBlock.Sources.Candels[i]
      else
        xCandel := FBlock.Vectors.Candels[i];
      case xCandel.Status of
        1: begin
          SetGrid(StringGridSource,xRow,xCandel);
          Inc(xRow);
        end;
        2: begin
          SetGrid(StringGridSource,xRow,xCandel);
          xRow := 0;
        end;
        3: begin
          SetGrid(StringGridVector,xRow,xCandel);
          Inc(xRow);
        end;
      end;
    end;
end;

end.
