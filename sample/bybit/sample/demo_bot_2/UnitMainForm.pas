unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Edit, FMXTee.Engine, FMXTee.Series, FMXTee.Procs, FMXTee.Chart,
  FMX.Layouts,
  Lb.Mode,
  Lb.Position.Trade, System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid;

type
  TMainForm = class(TForm)
    ButtonStart: TButton;
    EditOpen: TEdit;
    EditHigh: TEdit;
    EditLow: TEdit;
    EditClose: TEdit;
    EditTime: TEdit;
    EditVol: TEdit;
    TextTitleTime: TText;
    Text2: TText;
    Text3: TText;
    Text4: TText;
    Text5: TText;
    Text6: TText;
    GridLayout: TGridPanelLayout;
    Text7: TText;
    TextMode: TText;
    StrGrid: TStringGrid;
    EditProfit: TEdit;
    TextUpDataConnect: TText;
    Timer: TTimer;
    procedure ButtonStartClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    Mode: TMode;
    StartTime: String;
    CountNewBar: Integer;

    UpDataConenct: UInt64;
    OldUpDataConenct: UInt64;

    BybitKline: TBybitKline;
    procedure BybitKlineOnNewCandel(
      ATime: Int64;
      AOpen, AHigh, ALow, AClose: Double;
      AVoluem: Double
    );
    procedure BybitKlineOnUpCandel(
      ATime: Int64;
      AOpen, AHigh, ALow, AClose: Double;
      AVoluem: Double
    );
    procedure BybitKlineOnEventEndLoading(Sender: TObject);
    procedure CurrentPostionOnClose(Sender: TObject);
  private
    CurrentPrice: Double;
    CurrentPostion: TTradePostion;
    TradePostions: TTradePostions;
    procedure SetShowGrid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{$IFDEF DEBUG}
{$ENDIF}

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);

  procedure _SetAddCol(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single = 80);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(AStrGrid);
    xCol.Header := AHeader;
    xCol.Parent := AStrGrid;
    xCol.Width := AWidth;
  end;


  procedure _SetHeaders(const AGrid: TStringGrid);
  begin
    _SetAddCol(AGrid,'ID',50);
    _SetAddCol(AGrid,'OpenTime',120);
    _SetAddCol(AGrid,'CloseTime',120);
    _SetAddCol(AGrid,'Open');
    _SetAddCol(AGrid,'Close');
    _SetAddCol(AGrid,'Mode');
    _SetAddCol(AGrid,'Quantity');
    _SetAddCol(AGrid,'Status');
    _SetAddCol(AGrid,'TakeProfit');
    _SetAddCol(AGrid,'StopLoss');
    _SetAddCol(AGrid,'PriceTP');
    _SetAddCol(AGrid,'PriceSL');
    _SetAddCol(AGrid,'Profit');
    _SetAddCol(AGrid,'StopProfit');
    _SetAddCol(AGrid,'MaxProfit');
    _SetAddCol(AGrid,'MinProfit');
    _SetAddCol(AGrid,'ProfitQuantity');
    _SetAddCol(AGrid,'CommissionValue');
    _SetAddCol(AGrid,'IsTakeProfit');
    _SetAddCol(AGrid,'IsStopLoss');
  end;

begin
  inherited;
  CountNewBar := 0;
  UpDataConenct := 0;

  StartTime := '';
  BybitKline := TBybitKline.Create;
  BybitKline.OnEventEndLoading := BybitKlineOnEventEndLoading;

  TradePostions := TTradePostions.Create;
  _SetHeaders(StrGrid);
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(TradePostions);
  FreeAndNil(BybitKline);
  inherited;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  TradePostions.Items.Clear;

  CountNewBar := 0;
  UpDataConenct := 0;

  StartTime := '';

  BybitKline.Category := TTypeCategory.tcLinear;
  BybitKline.Symbol := 'BTCUSDT';
  BybitKline.Interval := TTypeInterval.ti_1;
  BybitKline.Limit := 1;
  BybitKline.Start(500);

  CurrentPrice := 0;

  if not Timer.Enabled then
    Timer.Enabled := True;
end;

procedure TMainForm.BybitKlineOnEventEndLoading(Sender: TObject);
var
  xInd, iCount: Integer;
  xCandel: TCandelObject;
begin
  Inc(UpDataConenct);
  OldUpDataConenct := UpDataConenct;

  iCount := BybitKline.CandelObjects.Count;
  if iCount > 0 then
  begin
    xInd := iCount - 1;
    xCandel := BybitKline.CandelObjects[xInd];
    EditTime.Text  := xCandel.startTime;
    EditOpen.Text  := xCandel.openPrice;
    EditHigh.Text  := xCandel.highPrice;
    EditLow.Text   := xCandel.lowPrice;
    EditClose.Text := xCandel.closePrice;
    EditVol.Text   := xCandel.volume;
    if SameText(StartTime,xCandel.startTime) then
    begin
      BybitKlineOnUpCandel(
        StrToUInt64Def(xCandel.startTime,0),
        xCandel.Open,
        xCandel.High,
        xCandel.Low,
        xCandel.Close,
        xCandel.Vol
      );
    end else
    begin
      StartTime := xCandel.startTime;
      BybitKlineOnNewCandel(
        StrToUInt64Def(xCandel.startTime,0),
        xCandel.Open,
        xCandel.High,
        xCandel.Low,
        xCandel.Close,
        xCandel.Vol
      );
    end;
  end;
end;

procedure TMainForm.CurrentPostionOnClose(Sender: TObject);
begin
  CurrentPostion := nil;
end;

procedure TMainForm.BybitKlineOnNewCandel(ATime: Int64; AOpen, AHigh, ALow,
  AClose, AVoluem: Double);
begin
  Inc(CountNewBar);
  TextTitleTime.Text := 'Время: [' + CountNewBar.ToString + ']';
  TextUpDataConnect.Text := 'Коннект: ' + UpDataConenct.ToString;

  Mode := GetRandomMode;
  case Mode of
    tmNull: TextMode.Text := 'Нейтральное состояние';
    tmBuy : TextMode.Text := 'Покупка';
    tmSell: TextMode.Text := 'Продажа';
  end;

  if Assigned(CurrentPostion) then
  begin
    if CurrentPostion.Status = TTypeStatus.tsOpen then
    begin
      if CurrentPostion.Mode <> Mode then
      begin
        CurrentPostion.SetClose(
          ATime,
          AOpen
        );
      end;
    end;
  end;


  if (not Assigned(CurrentPostion)) and (Mode <> tmNull) then
    if CountNewBar > 1 then
    begin
      CurrentPostion := TradePostions.GetOpen(
        ATime,
        AClose,
        0.001,
        Mode,
        20,
        20
      );
      CurrentPostion.TypeStopLoss := TTypeStopLoss.slTrailingStop;
      CurrentPostion.Commission := 0.01;
      CurrentPostion.OnClose := CurrentPostionOnClose;


      if CurrentPostion.Mode = tmNull then
        raise Exception.Create('Error Message: ошибка');
      SetShowGrid;
    end;

end;

procedure TMainForm.BybitKlineOnUpCandel(ATime: Int64; AOpen, AHigh, ALow,
  AClose, AVoluem: Double);
begin
  CurrentPrice := AClose;
  TradePostions.SetUpData(
      ATime,
      AClose
  );
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  xS: String;
begin
  SetShowGrid;

  if CurrentPrice > 0 then
  begin
    xS := 'Профит: ' + TradePostions.GetProfit(CurrentPrice).ToString;
    EditProfit.Text := xS;
  end;

  if (UpDataConenct <> OldUpDataConenct) then
  begin
    BybitKline.Stop;

    BybitKline.Category := TTypeCategory.tcLinear;
    BybitKline.Symbol := 'BTCUSDT';
    BybitKline.Interval := TTypeInterval.ti_15;
    BybitKline.Limit := 1;
    BybitKline.Start(100);
  end;

  TextUpDataConnect.Text := 'Коннект: ' + UpDataConenct.ToString;
end;

procedure TMainForm.SetShowGrid;

  function _IfThen(AValue: Boolean): String;
  begin
    if AValue then
      Result := 'True'
    else
      Result := 'False'
  end;

  procedure _RowGrid(AGrid: TStringGrid; APostion: TTradePostion; ARowID: Integer);
  begin
    AGrid.Cells[0,ARowID]  := APostion.ID.ToString;
    AGrid.Cells[1,ARowID]  := FloatToStr(APostion.OpenTime);
    AGrid.Cells[2,ARowID]  := FloatToStr(APostion.CloseTime);
    AGrid.Cells[3 ,ARowID] := APostion.Open.ToString;
    AGrid.Cells[4 ,ARowID] := APostion.Close.ToString;
    AGrid.Cells[5 ,ARowID] := GetStrToMode(APostion.Mode);
    AGrid.Cells[6 ,ARowID] := APostion.Quantity.ToString;
    AGrid.Cells[7 ,ARowID] := GetStrToStaus(APostion.Status);
    AGrid.Cells[8 ,ARowID] := APostion.TakeProfit.ToString;
    AGrid.Cells[9 ,ARowID] := APostion.StopLoss.ToString;
    AGrid.Cells[10,ARowID] := APostion.PriceTP.ToString;
    AGrid.Cells[11,ARowID] := APostion.PriceSL.ToString;
    AGrid.Cells[12,ARowID] := APostion.Profit.ToString;
    AGrid.Cells[13,ARowID] := APostion.StopProfit.ToString;
    AGrid.Cells[14,ARowID] := APostion.MaxProfit.ToString;
    AGrid.Cells[15,ARowID] := APostion.MinProfit.ToString;
    AGrid.Cells[16,ARowID] := APostion.ProfitQuantity.ToString;
    AGrid.Cells[17,ARowID] := APostion.CommissionValue.ToString;
    AGrid.Cells[18,ARowID] := _IfThen(APostion.IsTakeProfit);
    AGrid.Cells[19,ARowID] := _IfThen(APostion.IsStopLoss);
  end;

  procedure _ClearGrid(AGrid: TStringGrid; ARowID: Integer);
  var
    i, iCount: Integer;
  begin
    iCount := AGrid.ColumnCount;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
        AGrid.Cells[i,ARowID] := '';
  end;


var
  xPostion: TTradePostion;
  i, iCount: Integer;
begin
  iCount := TradePostions.Items.Count;
  if iCount > 0 then
  begin
    StrGrid.RowCount := iCount;
    for i := 0 to iCount - 1 do
    begin
      xPostion := TradePostions.Items[iCount - 1 - i];
      _RowGrid(StrGrid,xPostion,i);
      if i >= (StrGrid.RowCount - 1) then
        Break;
    end;
  end;
end;



end.
