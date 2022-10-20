unit UnitGridFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Grid, FMX.Objects, FMX.Layouts,
  Lb.SysUtils.Candel;

type
  TGirdFrame = class(TFrame)
    StringGridSource: TStringGrid;
    ColumnSourceDate: TStringColumn;
    ColumnSourceTime: TStringColumn;
    ColumnSourceOpen: TStringColumn;
    ColumnSourceHigh: TStringColumn;
    ColumnSourceLow: TStringColumn;
    ColumnSourceClose: TStringColumn;
    ColumnSourceVol: TStringColumn;
    StringGridFuture: TStringGrid;
    ColumnFutureDate: TStringColumn;
    ColumnFutureTime: TStringColumn;
    ColumnFutureOpen: TStringColumn;
    ColumnFutureHigh: TStringColumn;
    ColumnFutureLow: TStringColumn;
    ColumnFutureClose: TStringColumn;
    ColumnFutureVol: TStringColumn;
    LayoutSource: TLayout;
    LayoutFurute: TLayout;
    TextSourceText: TText;
    TextFutureText: TText;
  private
    FCandelsSource: TCandelList;
    FCandelsFuture: TCandelList;
  protected
    procedure SetCandelGrid(const AGrid: TStringGrid; const ACandels: TCandelList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetCandels(ASource, AFuture: TCandelList);
  end;

implementation

{$R *.fmx}

{ TGirdFrame }

constructor TGirdFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TGirdFrame.Destroy;
begin

  inherited;
end;

procedure TGirdFrame.SetCandels(ASource, AFuture: TCandelList);
begin
  FCandelsSource := ASource;
  FCandelsFuture := AFuture;

  TextSourceText.Text := 'Количество: ' + IntToStr(FCandelsSource.Count);
  SetCandelGrid(StringGridSource,FCandelsSource);

  TextFutureText.Text := 'Количество: ' + IntToStr(FCandelsFuture.Count);
  SetCandelGrid(StringGridFuture,FCandelsFuture);
end;

procedure TGirdFrame.SetCandelGrid(const AGrid: TStringGrid; const ACandels: TCandelList);
var
  xCandel: TCandel;
  i, iCount: Integer;
begin
  iCount := ACandels.Count;
  AGrid.RowCount := iCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel := ACandels[i];
      AGrid.Cells[0,i] := DateToStr(xCandel.Date);
      AGrid.Cells[1,i] := TimeToStr(xCandel.Time);
      AGrid.Cells[2,i] := FloatToStr(xCandel.Open);
      AGrid.Cells[3,i] := FloatToStr(xCandel.High);
      AGrid.Cells[4,i] := FloatToStr(xCandel.Low);
      AGrid.Cells[5,i] := FloatToStr(xCandel.Close);
      AGrid.Cells[6,i] := FloatToStr(xCandel.Vol);
    end;
end;

end.
