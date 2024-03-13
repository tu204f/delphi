unit UnitMainForm;

interface

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
  FMX.Layouts,
  FMX.Objects,

  Lb.Setting,

  Lb.Bybit.RecentTrade,
  Lb.Bybit.InstrumentsInfo,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Test,
  Lb.Bybit.Candels,

  Lb.SysUtils.Candel,
  Lb.Bot.Tiket,
  UnitChartFrame,

  System.Generics.Collections,
  System.Rtti,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Grid;

type
  TInfoTrade = record
    Symbol: String;
    Category: TTypeCategory;
    LimitCandel: Integer;
  end;

  TMainForm = class(TForm)
    LayoutButtom: TLayout;
    LayoutClient: TLayout;
    ButtonStart: TButton;
    ButtonStop: TButton;
    ButtonTrade: TButton;
    StatusText: TText;
    RectangleChar: TRectangle;
    TextLastPrice: TText;
    StringGrid: TStringGrid;
    LayoutInfoBot: TLayout;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonTradeClick(Sender: TObject);
  private {Управление}
    FIsActive: Boolean;
    procedure SetIsActive(const Value: Boolean);
  protected {Проверка соединение}
    BybitTest: TBybitTest;
    procedure BybitTestOnEventSing(Sender: TObject);
    procedure BybitTestOnEventTimer(Sender: TObject);
  protected {Параметры инструмента}
    LinearObject: TLinearObject;
    BybitInstrumentsInfo: TBybitInstrumentsInfo;
    procedure BybitInstrumentsInfoOnEventEndLoading(Sender: TObject);
  protected {Вывод графика}
    ChartFrame: TChartFrame;
    BybitCandels: TBybitCandels;
    procedure BybitCandelsChange(Sender: TObject);
    procedure BybitCandelsNewCandel(Sender: TObject);
  protected {тиковые данные}
    FRecentTrade: TRecentTradeObject;
    BybitRecentTrade: TBybitRecentTrade;
    procedure BybitRecentTradeEventEndLoading(Sender: TObject);
  protected {торговый робоы - эмитация, как вариант сделать массив ботов}
    Bot: TTakeProfitTiketBot;
    procedure SetNewCandel(const APrice: Double);
    procedure SetUpCandel(const APrice: Double);
    procedure BotOpenPosition(const ASander: TObject; AMode: TMode; APrice: Double);
    procedure BotClosePosition(const ASander: TObject; AMode: TMode; APrice: Double);
    procedure ShowGridBots;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IsActive: Boolean read FIsActive write SetIsActive;
  end;

var
  MainForm: TMainForm;
  InfoTrade: TInfoTrade;

implementation

{$R *.fmx}

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);

  procedure _AddCol(const AHeader: String; const AWidth: Single);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(StringGrid);
    xCol.Parent := StringGrid;
    xCol.Width := AWidth;
    xCol.Header := AHeader;
  end;

begin
  inherited Create(AOwner);

  FRecentTrade := nil;

  FIsActive := False;

  // Информация инструменте
  InfoTrade.Symbol := TSetting.ReadString('config.sys.symbol','BTCUSDT');
  InfoTrade.Category := GetTypeCategoryToStr(
    TSetting.ReadString('config.sys.category','linear')
  );
  InfoTrade.LimitCandel := TSetting.ReadInteger('config.sys.limit_candel',5);

  BybitTest := TBybitTest.Create;
  BybitTest.OnEventSing := BybitTestOnEventSing;
  BybitTest.OnEventTimer := BybitTestOnEventTimer;

  LinearObject := nil;
  BybitInstrumentsInfo := TBybitInstrumentsInfo.Create;
  BybitInstrumentsInfo.OnEventEndLoading := BybitInstrumentsInfoOnEventEndLoading;

  ChartFrame := TChartFrame.Create(nil);
  ChartFrame.Align := TAlignLayout.Client;
  ChartFrame.Parent := RectangleChar;
  ChartFrame.Capacity := InfoTrade.LimitCandel;

  BybitCandels := TBybitCandels.Create;
  BybitCandels.OnChange := BybitCandelsChange;
  BybitCandels.OnNewCandel := BybitCandelsNewCandel;

  BybitRecentTrade := TBybitRecentTrade.Create;
  BybitRecentTrade.OnEventEndLoading := BybitRecentTradeEventEndLoading;

  Bot := TTakeProfitTiketBot.Create;
  Bot.OnOpenPosition := BotOpenPosition;
  Bot.OnClosePosition := BotClosePosition;

  _AddCol('ID',50);
  _AddCol('SL',50);
  _AddCol('TP',50);
  _AddCol('Count',80);
  _AddCol('HP',100);

end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(Bot);
  FreeAndNil(BybitRecentTrade);
  FreeAndNil(ChartFrame);
  FreeAndNil(BybitCandels);
  FreeAndNil(BybitTest);
  FreeAndNil(ChartFrame);
  FreeAndNil(BybitTest);
  inherited;
end;


procedure TMainForm.SetIsActive(const Value: Boolean);
begin
  FIsActive := Value;
  ButtonStart.Enabled := not FIsActive;
  ButtonStop.Enabled := FIsActive;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  // Старт - бота
  if not IsActive then
  begin
    // Проверка соединение
    BybitTest.Start;

    // Загрузить график
    BybitCandels.Start(
      InfoTrade.Symbol,
      InfoTrade.LimitCandel,
      InfoTrade.Category,
      TTypeInterval.ti_1
    );

    BybitRecentTrade.Category := InfoTrade.Category;
    BybitRecentTrade.Symbol := InfoTrade.Symbol;
    BybitRecentTrade.Limit := 10;
    BybitRecentTrade.Start(100);


    // Информация по инструменту
    BybitInstrumentsInfo.Selected;
    BybitInstrumentsInfo.Category := InfoTrade.Category;
    BybitInstrumentsInfo.Symbol := InfoTrade.Symbol;

    Bot.Default;
    Bot.StopLoss := -10;
    Bot.TakeProfit := 50;
    Bot.CountStop := 3;

    Self.IsActive := True;
  end;
end;


procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  // Стоп - бот
  if IsActive then
  begin
    BybitTest.Stop;
    BybitCandels.Stop;
    BybitRecentTrade.Stop;
    Self.IsActive := False;
  end;
end;

procedure TMainForm.ButtonTradeClick(Sender: TObject);
begin
  // Показать проведенные сделки
end;

procedure TMainForm.BybitTestOnEventSing(Sender: TObject);
begin
  // Переустановить соединение,
end;

procedure TMainForm.BybitTestOnEventTimer(Sender: TObject);
var
  xS: String;
begin
  if Assigned(LinearObject) then
  begin
    xS :=
      'Время: ' + TimeToStr(BybitTest.TimeSecond) + '; ' +
      'Symbol: ' + LinearObject.Symbol + '; ' +
      'LeverageStep: ' + LinearObject.LeverageFilter.LeverageStep + '; ' +
      'TickSize: ' + LinearObject.PriceFilter.TickSize + '; ' +
      'QtyStep: ' + LinearObject.LotSizeFilter.QtyStep;
    StatusText.Text  := xS;
  end
  else
    StatusText.Text := 'Время соединение: ' + TimeToStr(BybitTest.TimeSecond);
  ShowGridBots;
end;

procedure TMainForm.BybitInstrumentsInfoOnEventEndLoading(Sender: TObject);
begin
  LinearObject := nil;
  if BybitInstrumentsInfo.LinearObjects.Count > 0 then
    LinearObject := BybitInstrumentsInfo.LinearObjects.Items[0];
end;

procedure TMainForm.BybitCandelsChange(Sender: TObject);

  procedure _ChartFrame;
  var
    xCandel:  TCandel;
    i, iCount: Integer;
  begin
    ChartFrame.Candels.Clear;
    iCount := BybitCandels.Sources.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel := BybitCandels.Sources[i];
        ChartFrame.Candels.Add(xCandel);
      end;
    ChartFrame.Build;
  end;

begin
  // Вывода графика
  _ChartFrame;
  if Assigned(FRecentTrade) then
    SetUpCandel(FRecentTrade.Price);
end;

procedure TMainForm.BybitCandelsNewCandel(Sender: TObject);
begin
  if Assigned(FRecentTrade) then
    SetNewCandel(FRecentTrade.Price);
end;

procedure TMainForm.BybitRecentTradeEventEndLoading(Sender: TObject);
var
  iCount: Integer;
begin
  // Последние сделки которые прошли
  iCount := BybitRecentTrade.RecentTrades.Count;
  if iCount > 0 then
  begin
    // Цена последний заявки
    FRecentTrade := BybitRecentTrade.RecentTrades.Items[0];
    TextLastPrice.Text :=
      'Время:' + FRecentTrade.Time.ToString + ' ' +
      'Price: ' + FRecentTrade.Price.ToString + ' ' +
      'Size: ' + FRecentTrade.Size.ToString;
  end;
end;

procedure TMainForm.ShowGridBots;
begin
  StringGrid.RowCount := 1;

  StringGrid.Cells[0,0] := Bot.ID.ToString;
  StringGrid.Cells[1,0] := Bot.StopLoss.ToString;
  StringGrid.Cells[2,0] := Bot.TakeProfit.ToString;
  StringGrid.Cells[3,0] := Bot.CountStop.ToString;
  StringGrid.Cells[4,0] := Bot.HealthPoints.ToString;
end;

procedure TMainForm.SetNewCandel(const APrice: Double);
begin
  if not Bot.IsPosition then
  begin
    Bot.SetMode;
    Bot.SetOpenPosition(APrice);
  end;
end;

procedure TMainForm.SetUpCandel(const APrice: Double);
begin
  // обновление значение цены
  Bot.SetUpPosition(APrice);
end;

procedure TMainForm.BotOpenPosition(const ASander: TObject; AMode: TMode; APrice: Double);
begin
  // Открытие позиции инструмента
end;

procedure TMainForm.BotClosePosition(const ASander: TObject; AMode: TMode; APrice: Double);
begin
  // Закрытие позиции инструмента
end;

end.
