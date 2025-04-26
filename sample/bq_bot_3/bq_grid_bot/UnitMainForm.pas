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
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.TabControl,

  Lb.Platform,
  Lb.Platform.Bybit,
  Lb.Bybit.SysUtils,
  Lb.SysUtils,

  UnitHistoryFrame,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo, FMX.Layouts;

type
  TMainForm = class(TForm)
    ButtonStartOrStop: TButton;
    Rectangle: TRectangle;
    TextStatus: TText;
    TabControl: TTabControl;
    TabItemHistory: TTabItem;
    MemoLog: TMemo;
    GridLayoutStatus: TGridPanelLayout;
    TextVolatility: TText;
    procedure ButtonStartOrStopClick(Sender: TObject);
  private
    HistoryFrame: THistoryFrame;
    TradingPlatform: TTradingPlatform;
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
    procedure TradingPlatformOnNewCandel(Sender: TObject);
    procedure TradingPlatformOnMsgInfo(ASender: TObject; AMsg: String);
    procedure TradingPlatformOnOrderBook(ASender: TObject;  APriceAsk, APriceBid: Double);
  protected
    procedure DoStart;
    procedure DoStop;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  HistoryFrame := THistoryFrame.Create(Self);
  HistoryFrame.Parent := TabItemHistory;
  HistoryFrame.Align := TAlignLayout.Client;

  // *************************************************************************
  // Реализация торговой платформы
  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;
  TradingPlatform.OnNewCandel   := TradingPlatformOnNewCandel;
  TradingPlatform.OnMsgInfo     := TradingPlatformOnMsgInfo;
  TradingPlatform.OnOrderBook   := TradingPlatformOnOrderBook;
  // 'DdncwwQY6AVdShL008';
  // 'ldfYDnYhlVU5SU7w89mOnaHi0icy8XctNXtT';
  TPlatfomBybit(TradingPlatform).ApiKey := 't0YI4Ou0TKOTd7WrkE';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'dWcdTGIulDoKOiK4mggPQIkYwmMFGxvFVusp';
  TPlatfomBybit(TradingPlatform).Interval  := TTypeInterval.ti_15;

end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(TradingPlatform);
  FreeAndNil(HistoryFrame);
  inherited;
end;


procedure TMainForm.ButtonStartOrStopClick(Sender: TObject);
begin
  if TradingPlatform.IsActive then
    DoStop
  else
    DoStart;
end;

procedure TMainForm.DoStart;
begin
  if not TradingPlatform.IsActive then
  begin
    ButtonStartOrStop.Text := 'Стоп';
    TradingPlatform.Symbol := 'ETHUSDT';
    TradingPlatform.StateMarket.Qty := 0.1;
    TradingPlatform.Start;
  end;
end;

procedure TMainForm.DoStop;
begin
  if TradingPlatform.IsActive then
  begin
    ButtonStartOrStop.Text := 'Старт';
    TradingPlatform.Stop;
  end;
end;

procedure TMainForm.TradingPlatformOnMsgInfo(ASender: TObject; AMsg: String);
begin
  MemoLog.Lines.Add(AMsg);
end;

procedure TMainForm.TradingPlatformOnNewCandel(Sender: TObject);
begin

end;

procedure TMainForm.TradingPlatformOnOrderBook(ASender: TObject; APriceAsk, APriceBid: Double);
begin
  // Лучьшии цены
  TextStatus.Text := 'Цена: ' + APriceAsk.ToString + '/' + APriceBid.ToString;
end;

procedure TMainForm.TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
begin
  // Исторические данные
  TextVolatility.Text := 'Волатильность: ' + TradingPlatform.ValueVolatility.DeviationLow.ToString;
  HistoryFrame.SetUpData(AStateMarket.Candels);
end;

end.
