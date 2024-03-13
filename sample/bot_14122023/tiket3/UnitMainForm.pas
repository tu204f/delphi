unit UnitMainForm;

interface

{$I tiket.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  System.Rtti,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Objects,
  FMX.Layouts,
  FMX.ListBox,
  Lb.SysUtils,
  Lb.ReadPrice,
  Lb.Bot.Tiket,
  Lb.Position.Trade, FMXTee.Engine, FMXTee.Series, FMXTee.Procs, FMXTee.Chart,
  FMX.TabControl;

type
  TMainForm = class(TForm)
    ButtonConnect: TButton;
    Timer: TTimer;
    TextStatus: TText;
    ButtonBuy: TButton;
    ButtonSell: TButton;
    Button1: TButton;
    TextProfit: TText;
    StrGridR: TStringGrid;
    RealGrid: TStringGrid;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Chart1: TChart;
    Series1: TLineSeries;
    Chart2: TChart;
    LineSeries1: TLineSeries;
    Series2: TLineSeries;
    procedure ButtonConnectClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    IsNewBar: Boolean;
    IndexTiket: Integer;
    BeginTime: Int64;
    SourceTikets : TSourceTikets;
    TradePostions: TTradePostions;
    RealPostions : TTradePostions;
  public
    BufferTikets: TBufferTikets;
  public
    OldProfit  : Double;
    RealCount  : Integer;
    BuyPostion : TTradePostion;
    SellPostion: TTradePostion;
    RealPostion: TTradePostion;
    procedure VirtualPostionClose(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetUpData(
      const ATime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
      const APrice: Double
    );
    procedure SetShowGrid;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  UnitLogForm,
  System.DateUtils;

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
    _SetAddCol(AGrid,'MaxProfit');
    _SetAddCol(AGrid,'MinProfit');
    _SetAddCol(AGrid,'ProfitQuantity');
    _SetAddCol(AGrid,'CommissionValue');
    _SetAddCol(AGrid,'IsTakeProfit');
    _SetAddCol(AGrid,'IsStopLoss');
  end;

begin
  inherited Create(AOwner);
  SourceTikets := nil;

  _SetHeaders(StrGridR);
  _SetHeaders(RealGrid);

  TradePostions := TTradePostions.Create;
  RealPostions := TTradePostions.Create;

  BufferTikets := TBufferTikets.Create;

end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(RealPostions);
  FreeAndNil(TradePostions);

  if Assigned(SourceTikets) then
    FreeAndNil(SourceTikets);

  FreeAndNil(BufferTikets);

  inherited;
end;


procedure TMainForm.ButtonConnectClick(Sender: TObject);
var
  xPath: String;
begin
  OldProfit := 0;

  xPath := 'd:\work\git\delphi\sample\bot_14122023\data\SRH4202302_240115_240115.txt';
  SourceTikets := TSourceTikets.Create;
  SourceTikets.LoadFromFile(xPath);
  SourceTikets.First;

  Timer.Enabled := not Timer.Enabled;
  if Timer.Enabled then
  begin
    ButtonConnect.Text := 'Стоп';
  end
  else
  begin
    ButtonConnect.Text := 'Старт';
  end;
  {$IFDEF DEBUG}
  SetLog('Начинаем тестирования, загружаем тиковые данные');
  {$ENDIF}
end;


procedure TMainForm.TimerTimer(Sender: TObject);

  function _StrTime(ATime: Int64): String;
  begin
    Result := DateTimeToStr(UnixToDateTime(Trunc(ATime/1000)));
  end;

  procedure _Read;
  var
    xTiket: TTiket;
    xBeginTime, xEndTime: Int64;
  begin
    try
      xBeginTime := BeginTime;
      // Пять минут
      xEndTime   := xBeginTime ;
      xTiket := SourceTikets.Tiket;

      BufferTikets.SetTiket(xTiket.Last);
      Series2.Add(BufferTikets.PriceMA);
      LineSeries1.Add(xTiket.Last);

      if (xBeginTime >= xTiket.Time) and (xTiket.Time < xEndTime) then
      begin
        // Обновление
      end
      else
      begin
        // Новая свеча
        IsNewBar := True;
        BeginTime  := xEndTime;
      end;
      SetUpData(xTiket.Time,xTiket.Last);
      Inc(IndexTiket);
      SourceTikets.Next;
      if SourceTikets.Eof then
        Timer.Enabled := False;
    except
      Timer.Enabled := False;
    end;
  end;

var
  i, iCount: Integer;
begin
  iCount := 100;
  for i := 0 to iCount - 1 do
  begin
    _Read;
    if not Timer.Enabled then
      Break;
  end;

  TextStatus.Text := 'IndexTiket: ' + IndexTiket.ToString;
  Self.SetShowGrid;

  if Assigned(BuyPostion) and Assigned(SellPostion) then
    if (BuyPostion.Status = tsClose) and (SellPostion.Status = tsClose)  then
    begin
      {$IFDEF DEBUG}
      SetLog('Открываем виртуальные две заявки');
      {$ENDIF}
      ButtonBuyClick(nil);
      ButtonSellClick(nil);
    end;
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
    {$IFDEF RUB}
    AGrid.Cells[1,ARowID]  := FloatToStr(APostion.OpenTime);
    AGrid.Cells[2,ARowID]  := FloatToStr(APostion.CloseTime);
    {$ELSE}
    AGrid.Cells[1,ARowID]  := IntToStr(APostion.OpenTime);
    AGrid.Cells[2,ARowID]  := IntToStr(APostion.CloseTime);
    {$ENDIF}

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
    AGrid.Cells[13,ARowID] := APostion.MaxProfit.ToString;
    AGrid.Cells[14,ARowID] := APostion.MinProfit.ToString;
    AGrid.Cells[15,ARowID] := APostion.ProfitQuantity.ToString;
    AGrid.Cells[16,ARowID] := APostion.CommissionValue.ToString;
    AGrid.Cells[17,ARowID] := _IfThen(APostion.IsTakeProfit);
    AGrid.Cells[18,ARowID] := _IfThen(APostion.IsStopLoss);
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
  {$IFDEF RUB}
  xTiket: TTiket;
  {$ELSE}
  xTrade: TBybitSourceTikets.TTrade;
  {$ENDIF}
  xPostion: TTradePostion;
  i, iCount: Integer;
begin
  {$IFDEF RUB}
  xTiket := SourceTikets.Tiket;
  {$ELSE}
  xTrade := SourceTikets.Trade;
  {$ENDIF}


  RealGrid.RowCount := 10;
  iCount := RealPostions.Items.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPostion := RealPostions.Items[iCount - 1 - i];
      _RowGrid(RealGrid,xPostion,i);
      if i >= (RealGrid.RowCount - 1) then
        Break;
    end;


  StrGridR.RowCount := 3;
  if Assigned(BuyPostion) then
    _RowGrid(StrGridR,BuyPostion,0)
  else
    _ClearGrid(StrGridR,0);

  if Assigned(SellPostion) then
    _RowGrid(StrGridR,SellPostion,1)
  else
    _ClearGrid(StrGridR,1);

  if Assigned(RealPostion) then
    _RowGrid(StrGridR,SellPostion,2)
  else
    _ClearGrid(StrGridR,2);

  // Управление объектами
  var xS := 'Вер: ' + TradePostions.GetProfit(
    {$IFDEF RUB}
    xTiket.Last
    {$ELSE}
    xTrade.Price
    {$ENDIF}
  ).ToString;


  var xProfit := RealPostions.GetProfit(
    {$IFDEF RUB}
    xTiket.Last
    {$ELSE}
    xTrade.Price
    {$ENDIF}
  );
  xS := xS + ' Реал: ' + xProfit.ToString;

  if OldProfit <> xProfit then
  begin
    Series1.Add(xProfit);
    OldProfit := xProfit;
  end;

  TextProfit.Text := xS;
end;

procedure TMainForm.SetUpData(const ATime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF}; const APrice: Double);
  {реальный операции прописать}

  procedure _UpDataPosition(
    const APostion: TTradePostion;
    const ATime: Int64;
    const APrice: Double
  );
  begin
    try
      if Assigned(APostion) then
      begin
        if APostion.Status = tsOpen then
        begin
          // Проверяем допольнительное условие закрытие позции
          if (APostion.Profit > 0) and  (APostion.MinProfit < 0) then
            APostion.SetClose(ATime, APrice);
          // Допольнительное условие закрытие
          if (APostion.MaxProfit - APostion.Profit) > 100 then
            APostion.SetClose(ATime, APrice);
        end;
      end;
    except
      on E: Exception do
      begin
        var xS := '_UpDataPosition:' + E.Message;
        ShowMessage(xS);
      end;
    end;
  end;

var
  xMode: TMode;
  xTime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
  xPrice: Double;
begin
  TradePostions.SetUpData(ATime,APrice);
  RealPostions.SetUpData(ATime,APrice);

  //_UpDataPosition(BuyPostion,ATime,APrice);
  //_UpDataPosition(SellPostion,ATime,APrice);
  //_UpDataPosition(RealPostion,ATime,APrice);

  if (not Assigned(BuyPostion)) and (not Assigned(SellPostion)) then
    Exit;
end;


procedure TMainForm.Button1Click(Sender: TObject);
begin
  ButtonBuyClick(nil);
  ButtonSellClick(nil);
end;

procedure TMainForm.ButtonBuyClick(Sender: TObject);
var
  xTrade: {$IFDEF RUB} TTiket {$ELSE} TBybitSourceTikets.TTrade {$ENDIF};
begin
  if Assigned(BuyPostion) and (BuyPostion.Status = tsClose) then
    BuyPostion := nil;

  xTrade := SourceTikets.Tiket;
  if not Assigned(BuyPostion) then
  begin
    {$IFDEF DEBUG}
    SetLog('  >> ' +
      {$IFDEF RUB}
      TimeToStr(xTrade.Time) + ' ' +
      xTrade.Last.ToString + ' vr.покупка');
      {$ELSE}
      TimeToStr(xTrade._Time) + ' ' +
      xTrade.Price.ToString + ' vr.покупка');
      {$ENDIF}
    {$ENDIF}
    BuyPostion := TradePostions.GetOpen(
        xTrade.Time,
        {$IFDEF RUB} xTrade.Last {$ELSE} xTrade.Price {$ENDIF},
        1,
        tmBuy,
        40,
        10
      );
    BuyPostion.OnClose := VirtualPostionClose;
  end;
end;

procedure TMainForm.ButtonSellClick(Sender: TObject);
var
  xTrade: {$IFDEF RUB} TTiket {$ELSE} TBybitSourceTikets.TTrade {$ENDIF};
begin
  if Assigned(SellPostion) and (SellPostion.Status = tsClose) then
    SellPostion := nil;

  xTrade := {$IFDEF RUB} SourceTikets.Tiket {$ELSE} SourceTikets.Trade {$ENDIF};
  if not Assigned(SellPostion) then
  begin
    {$IFDEF DEBUG}
    SetLog('  >> ' +
      {$IFDEF RUB}
      TimeToStr(xTrade.Time) + ' ' +
      xTrade.Last.ToString + ' vr.продажа');
      {$ELSE}
      TimeToStr(xTrade._Time) + ' ' +
      xTrade.Price.ToString + ' vr.продажа');
      {$ENDIF}
    {$ENDIF}
    SellPostion := TradePostions.GetOpen(
        xTrade.Time,
        {$IFDEF RUB} xTrade.Last {$ELSE} xTrade.Price {$ENDIF},
        1,
        tmSell,
        40,
        10
      );
    SellPostion.OnClose := VirtualPostionClose;
  end;
end;

procedure TMainForm.VirtualPostionClose(Sender: TObject);

  procedure _OpenReal(APostion: TTradePostion);
  var
    xMode: TMode;
  begin
    case APostion.Mode of
      tmBuy : xMode := tmSell;
      tmSell: xMode := tmBuy;
    else
      Exit;
    end;

    if not (xMode = tmNull) then
    begin
      if RealCount > 0 then
      begin
        RealCount := RealCount - 1;
        Exit;
      end;
      {$IFDEF DEBUG}
      SetLog(' >> Открытие реальной позиции');
      {$ENDIF}
      if Assigned(RealPostion) and (RealPostion.Status = tsClose) then
        RealPostion := nil;
      if not Assigned(RealPostion) then
      begin
        RealPostion := RealPostions.GetOpen(
          APostion.CloseTime,
          APostion.Close,
          1,
          xMode,
          120,
          30
        );
        RealPostion.Commission := 1;
      end;
    end;
  end;

  procedure _CloseReal(APostion: TTradePostion);
  begin
    {$IFDEF DEBUG}
    SetLog('  >> закрываем реальную позицию');
    {$ENDIF}
    if Assigned(RealPostion) then
    begin
      if RealPostion.Status = tsOpen then
      begin
        RealPostion.SetClose(
          APostion.CloseTime,
          APostion.Close
        );
      end;


      if RealPostion.Profit <= 0 then
        RealCount := 0;

      RealPostion := nil;
    end;
  end;

var
  xPostion: TTradePostion;
begin
  xPostion := TTradePostion(Sender);
  if Assigned(RealPostion) then
    _CloseReal(xPostion)
  else
    _OpenReal(xPostion);
end;

end.
