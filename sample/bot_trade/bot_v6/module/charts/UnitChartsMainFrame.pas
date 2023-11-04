unit UnitChartsMainFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  Lb.SysUtils.Candel,
  Lb.Module.SysUtils,
  FMX.Layouts,
  FMX.Objects,
  UnitChartCandelsFrame,
  FMX.ListBox,
  Lb.Trader, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.Rtti, FMX.Grid.Style, FMX.Grid;

const
  ///<summary>Коэффициент единичного пространство</summary>
  SINGLE_СAST = 10;

  ///<summary>Размер буфера свечей</summary>
  SIZE_BUFFER_CANDEL = 50;

type
  ///<summary>Визуализации данных</summary>
  TChartsMainFrame = class(TFrame,IModule)
    Layout1: TLayout;
    ListBox1: TListBox;
    StringGrid1: TStringGrid;
    StringColumnTime: TStringColumn;
    StringColumnPrice: TStringColumn;
    StringColumnQuantity: TStringColumn;
    StringColumnBuySell: TStringColumn;
    StringColumnST: TStringColumn;
    StringColumnTP: TStringColumn;
    StringColumnStatus: TStringColumn;
  private
    FMemoryCandels: TMemoryCandels;
  private
    FChartCandelsFrame: TChartCandelsFrame;
  protected
    IndexInc: Integer;
    function Start: Boolean;
    function Stop: Boolean;
    function UpData: Boolean;
    function GetCaption: WideString;
  protected
    procedure DoBoxCandels;
    property MemoryCandels: TMemoryCandels read FMemoryCandels;
    procedure EventIntersection(Sender: TObject; ATypeLine: TTypeLine; APrice: Double);
  public
    Trades: TTrades;
    LevelSR: TLevelSR;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses
  Lb.Setting;

///<summary>Переводи вектора реальности в едичное пространство</summary>
procedure SingleСastCandels(const ASource, ASingle: TCandels; const AValueSingle: Integer = SINGLE_СAST);

  function _GetY(const AValueSingle: Integer; const APrice, AMaxPrice, AMinPrice: Double): Integer;
  begin
    Result := 0;
    if AValueSingle <= 0 then
      Exit;
    var xDeltaPrice := AMaxPrice - AMinPrice;
    if xDeltaPrice > 0 then
    begin
      var xDeltaPoint := xDeltaPrice/AValueSingle;
      Result := Trunc((APrice - AMinPrice)/xDeltaPoint);
    end
    else
    begin
      var xS := 'MaxPrice: ' + FloatToStr(AMaxPrice) + '; MinPrice: ' + FloatToStr(AMinPrice);
      raise Exception.Create('Error Message: Не верное значение ' + xS);
    end;
  end;

  function _ToCandel(const ACandel: TCandel; const AValueSingle: Integer; const AMaxPrice, AMinPrice: Double): TCandel;
  var
    xCandel: TCandel;
  begin
    xCandel.NullValue;
    if not ACandel.IsEmptyPrice then
    begin
      xCandel.Open  := _GetY(AValueSingle,ACandel.Open ,AMaxPrice, AMinPrice);
      xCandel.High  := _GetY(AValueSingle,ACandel.High ,AMaxPrice, AMinPrice);
      xCandel.Low   := _GetY(AValueSingle,ACandel.Low  ,AMaxPrice, AMinPrice);
      xCandel.Close := _GetY(AValueSingle,ACandel.Close,AMaxPrice, AMinPrice);
      xCandel.Vol   := ACandel.Vol;
    end;
    Result := xCandel;
  end;

var
  xCandel, xSourcexCandel: TCandel;
  xMaxPrice, xMinPrice: Double;
begin
  ASingle.Clear;
  xMaxPrice := ASource.MaxPrice;
  xMinPrice := ASource.MinPrice;
  if xMaxPrice > xMinPrice  then
  begin
    for xSourcexCandel in ASource do
    begin
      //xCandel := xSourcexCandel;
      xCandel := _ToCandel(xSourcexCandel,AValueSingle,xMaxPrice, xMinPrice);
      ASingle.Add(xCandel);
    end;
  end
  else
  begin
    var xS := 'MaxPrice: ' + FloatToStr(xMaxPrice) + '; MinPrice: ' + FloatToStr(xMinPrice);
    raise Exception.Create('Error Message: Не верное значение ' + xS);
  end;
end;

{ TChartsMainFrame }

constructor TChartsMainFrame.Create(AOwner: TComponent);
begin
  inherited;
  FMemoryCandels := TMemoryCandels.Create;

  FChartCandelsFrame := TChartCandelsFrame.Create(nil);
  FChartCandelsFrame.Parent := Layout1;
  FChartCandelsFrame.Align := TAlignLayout.Client;

  LevelSR := TLevelSR.Create;
  LevelSR.OnEventIntersection := EventIntersection;

  Trades := TTrades.Create;
end;

destructor TChartsMainFrame.Destroy;
begin
  FreeAndNil(Trades);
  FreeAndNil(LevelSR);
  FreeAndNil(FChartCandelsFrame);
  FreeAndNil(FMemoryCandels);
  inherited;
end;

function TChartsMainFrame.GetCaption: WideString;
begin
  Result := 'Свячной график';
end;

function TChartsMainFrame.Start: Boolean;
begin
  IndexInc := 0;
  Result := True;
  try
    MemoryCandels.FileName := TSetting.ReadString(CONFIG_FILE_NAME);
    MemoryCandels.CandelFirst;
  except
    Result := False;
  end;
  ListBox1.Clear;
end;

function TChartsMainFrame.Stop: Boolean;
var
  xStr: TStringList;
begin
  Result := True;

  //
  xStr := TStringList.Create;
  try
    for var xTrade in Trades.TradeClose  do
    begin
      var xS := '';
      xS := xS + DateToStr(xTrade.Date) + ';';
      xS := xS + TimeToStr(xTrade.Time) + ';';
      xS := xS + xTrade.IndexInc.ToString + ';';
      xS := xS + xTrade.Potential.ToString + ';';
      xS := xS + xTrade.Price.ToString + ';';
      xS := xS + xTrade.Quantity.ToString + ';';
      xS := xS + xTrade.BuySell + ';';
      xS := xS + xTrade.SL.ToString + ';';
      xS := xS + xTrade.TP.ToString + ';';
      xS := xS + xTrade.Close_IndexInc.ToString + ';';
      xS := xS + DateToStr(xTrade.Close_Date) + ';';
      xS := xS + TimeToStr(xTrade.Close_Time) + ';';
      xS := xS + xTrade.Close_Potential.ToString + ';';
      xS := xS + xTrade.Status.ToString + ';';
      xS := xS + xTrade.Profit.ToString;

      xStr.Add(xS);
    end;
  finally
    xStr.SaveToFile('result.csv');
    FreeAndNil(xStr);
  end;

  ListBox1.Items.SaveToFile('data_result.csv')
end;

function TChartsMainFrame.UpData: Boolean;

  procedure _AddBufferCandels(const ACandel: TCandel);
  begin
    LevelSR.AddBufferCandel(ACandel);
  end;

var
  xNext: Boolean;
  xCandel: TCandel;
  //xSupport, xResistance: Double;
begin
  Inc(IndexInc);
  //Result := True;
  xNext :=  not MemoryCandels.CandelEOF;
  if xNext then
  begin
    xCandel := MemoryCandels.Candel;
    _AddBufferCandels(xCandel);

    Trades.UpDate(
      xCandel.Date,
      xCandel.Time,
      IndexInc,
      xCandel.Close,
      LevelSR.Potential
    );

    MemoryCandels.CandelNext;
  end;

  ListBox1.Items.BeginUpdate;
  try
    with ListBox1.Items do
    begin
      Clear;
      Add('MaxPeriod := ' + LevelSR.MaxPeriod.ToString);
      Add('MinPeriod := ' + LevelSR.MinPeriod.ToString);
      Add('Step := ' + LevelSR.Step.ToString);
      Add('Support := ' + LevelSR.Support.ToString);
      Add('Resistance := ' + LevelSR.Resistance.ToString);
      Add('PeriodSupport := ' + LevelSR.PeriodSupport.ToString);
      Add('PeriodResistance := ' + LevelSR.PeriodResistance.ToString);
    end;
  finally
    ListBox1.Items.EndUpdate;
  end;
  DoBoxCandels;
  Result := xNext;
end;

procedure TChartsMainFrame.DoBoxCandels;
begin
  FChartCandelsFrame.Candels.Copy(LevelSR.BufferCandels);
  FChartCandelsFrame.SetSupportResistance(
    LevelSR.Support,
    LevelSR.Resistance
  );
  FChartCandelsFrame.BuildChart;
end;

procedure TChartsMainFrame.EventIntersection(Sender: TObject; ATypeLine: TTypeLine; APrice: Double);

  function _GetQuantity: Integer;
  var
    iCount: Integer;
    xTrade: TTrades.TTrade;
  begin
    Result := 1;
    iCount := Trades.TradeClose.Count;
    if iCount > 0 then
    begin
      xTrade := Trades.TradeClose[iCount - 1];
      if xTrade.Status = TTrades.ST_CLOSE_SL then
        Result := xTrade.Quantity * 2;
    end;
  end;

var
  xCandel: TCandel;
  xTrade: TTrades.TTrade;
begin
  if Trades.Posiotion = 0 then
  begin
    xCandel := MemoryCandels.Candel;
    case ATypeLine of
      TTypeLine.tlSupport: begin
        Trades.Operation(
          xCandel.Date,
          xCandel.Time,
          IndexInc,
          APrice,
          1,//_GetQuantity,
          'S',
          LevelSR.Potential);
      end;
      TTypeLine.tlResistance: begin
        Trades.Operation(
          xCandel.Date,
          xCandel.Time,
          IndexInc,
          APrice,
          1,//_GetQuantity,
          'B',
          LevelSR.Potential);
      end;
    end;
  end;

  StringGrid1.RowCount := Trades.Count;
  if Trades.Count > 0 then
  begin
    {todo: нужно будет расширеть значение объекта}
    for var i := 0 to Trades.Count - 1 do
    begin
      xTrade := Trades.Items[i];
      StringGrid1.Cells[0,i] := TimeToStr(xTrade.Time);
      StringGrid1.Cells[1,i] := xTrade.Price.ToString;
      StringGrid1.Cells[2,i] := xTrade.Quantity.ToString;
      StringGrid1.Cells[3,i] := xTrade.BuySell;
      StringGrid1.Cells[4,i] := xTrade.SL.ToString;
      StringGrid1.Cells[5,i] := xTrade.TP.ToString;
      StringGrid1.Cells[6,i] := xTrade.Status.ToString;
    end;
  end;


end;

initialization
  RegistrationFrame('CHARTS_CANDELS',TChartsMainFrame);

finalization

end.
