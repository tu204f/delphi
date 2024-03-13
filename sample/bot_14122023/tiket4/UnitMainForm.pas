unit UnitMainForm;

interface

{$I tiket.inc}

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
  Lb.Position.Trade,
  FMXTee.Engine,
  FMXTee.Series,
  FMXTee.Procs,
  FMXTee.Chart,
  FMX.TabControl;

type
  TDoubleList = TList<Double>;

  TMainForm = class(TForm)
    Timer: TTimer;
    StrGridR: TStringGrid;
    MenuLayout: TLayout;
    ButtonConnect: TButton;
    ButtonOpenPosition: TButton;
    TextStatus: TText;
    TextProfit: TText;
    ListLayout: TLayout;
    Layout2: TLayout;
    ListBoxFile: TListBox;
    RealGrid: TStringGrid;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Chart1: TChart;
    Series1: TLineSeries;
    TabItem2: TTabItem;
    Chart2: TChart;
    Series2: TLineSeries;
    Series3: TLineSeries;
    Series4: TLineSeries;
    Series5: TPointSeries;
    procedure ButtonConnectClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonOpenPositionClick(Sender: TObject);
  private
    { Private declarations }
  public
    IsNewBar: Boolean;
    IndexTiket: Integer;
    BeginTime: Int64;
    SourceTikets : TSourceTikets;
    Postions : TTradePostions;
  public
    ValueSeries1: TDoubleList;
    ValueSeries2: TDoubleList;

    BufferTikets: TBufferTikets;
  public
    OldProfit : Double;
    RealCount : Integer;
    Postion   : TTradePostion;
    procedure PostionClose(Sender: TObject);
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

  Postions := TTradePostions.Create;
  BufferTikets := TBufferTikets.Create;


  ValueSeries1 := TDoubleList.Create;
  ValueSeries2 := TDoubleList.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(ValueSeries1);
  FreeAndNil(ValueSeries2);

  FreeAndNil(Postions);

  if Assigned(SourceTikets) then
    FreeAndNil(SourceTikets);

  FreeAndNil(BufferTikets);

  inherited;
end;


procedure TMainForm.ButtonConnectClick(Sender: TObject);
var
  xPath: String;
begin
  ValueSeries1.Clear;
  ValueSeries2.Clear;

  IndexTiket := 0;
  Postions.Items.Clear;
  Series1.Clear;
  Series2.Clear;
  Series5.Clear;

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

  procedure _ShowValueSeries;
  var
    xV1, xV2: Double;
    i, iCount: Integer;
  begin
    Chart2.BeginUpdate;
    try
      Series2.Clear;
      Series5.Clear;
      Series3.Clear;
      Series4.Clear;

      iCount := ValueSeries1.Count;
      if iCount > 0 then
        for i := 0 to iCount - 1 do
        begin
          xV1 := ValueSeries1[i];
          xV2 := ValueSeries2[i];

          Series5.Add(xV1);
          Series2.Add(xV2);

          Series3.Add(xV2 + 20);
          Series4.Add(xV2 - 20);

        end;
    finally
      Chart2.EndUpdate;
    end;
  end;

  function _StrTime(ATime: Int64): String;
  begin
    Result := DateTimeToStr(UnixToDateTime(Trunc(ATime/1000)));
  end;

  procedure _Read;
  var
    xValueMA: Double;
    xTiket: TTiket;
    xBeginTime, xEndTime: Int64;
  begin
    try
      xBeginTime := BeginTime;
      // Пять минут
      xEndTime   := xBeginTime ;
      xTiket := SourceTikets.Tiket;

      BufferTikets.SetTiket(xTiket.Last);
      xValueMA := BufferTikets.PriceMA;


      ValueSeries1.Add(xTiket.Last);
      ValueSeries2.Add(xValueMA);
      if ValueSeries2.Count > 2000 then
      begin
        ValueSeries1.Delete(0);
        ValueSeries2.Delete(0);
      end;

      _ShowValueSeries;


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

  if Assigned(Postion) then
    if (Postion.Status = tsClose) then
    begin
      {$IFDEF DEBUG}
      SetLog('Открываем виртуальные две заявки');
      {$ENDIF}
      ButtonOpenPositionClick(nil);
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

  iCount := Postions.Items.Count;
  if iCount > 0 then
  begin
    RealGrid.RowCount := iCount;
    for i := 0 to iCount - 1 do
    begin
      xPostion := Postions.Items[iCount - 1 - i];
      _RowGrid(RealGrid,xPostion,i);
      if i >= (RealGrid.RowCount - 1) then
        Break;
    end;
  end;


  StrGridR.RowCount := 3;
  if Assigned(Postion) then
    _RowGrid(StrGridR,Postion,0)
  else
    _ClearGrid(StrGridR,0);

  var xProfit := Postions.GetProfit(
    {$IFDEF RUB}
    xTiket.Last
    {$ELSE}
    xTrade.Price
    {$ENDIF}
  );
  var xS := 'Профит: ' + xProfit.ToString;
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
  Postions.SetUpData(ATime,APrice);

  //_UpDataPosition(BuyPostion,ATime,APrice);
  //_UpDataPosition(SellPostion,ATime,APrice);
  //_UpDataPosition(RealPostion,ATime,APrice);

  if (not Assigned(Postion)) then
    Exit;
end;


procedure TMainForm.ButtonOpenPositionClick(Sender: TObject);
var
  xValueATR: Double;
  xTrade: {$IFDEF RUB} TTiket {$ELSE} TBybitSourceTikets.TTrade {$ENDIF};
  xMode: TMode;
begin

  xMode := GetRandomMode;
  if xMode = tmNull then
    xMode := TMode(Random(2) + 1);

  if Assigned(Postion) and (Postion.Status = tsClose) then
    Postion := nil;

  xTrade := SourceTikets.Tiket;
  if not Assigned(Postion) then
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

    Postion := Postions.GetOpen(
        xTrade.Time,
        {$IFDEF RUB} xTrade.Last {$ELSE} xTrade.Price {$ENDIF},
        1,
        xMode,
        20,
        20
      );
    Postion.Commission := 1;
    Postion.OnClose := PostionClose;
  end;
end;

procedure TMainForm.PostionClose(Sender: TObject);
begin
  // Результат
  // ButtonOpenPositionClick(nil);
end;

end.
