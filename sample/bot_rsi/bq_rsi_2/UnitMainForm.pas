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
  FMX.Memo.Types,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.StdCtrls,

  Lb.Buffer.Trading,
  Lb.SysUtils,
  Lb.Bot,
  Lb.Platform,
  Lb.Platform.Bybit,
  Lb.Category,
  FMX.TabControl,
  FMX.Layouts,

  System.Rtti,
  FMX.Grid.Style,
  FMX.Grid, FMX.Objects;

type
  TMainForm = class(TForm)
    ButtonStart: TButton;
    ButtonBuy: TButton;
    ButtonSell: TButton;
    ButtonBuy2: TButton;
    ButtonSell2: TButton;
    ButtonStop: TButton;
    TabControl: TTabControl;
    TabItemLog: TTabItem;
    TabItemFrame: TTabItem;
    MemoInfo: TMemo;
    GridLayout: TGridPanelLayout;
    LayoutSell: TLayout;
    LayoutBuy: TLayout;
    StrGrid: TStringGrid;
    Text1: TText;
    Timer: TTimer;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
    procedure ButtonBuy2Click(Sender: TObject);
    procedure ButtonSell2Click(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    procedure SetShowGrid;
  private
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
  public
    IndexTrade: Integer;
    ManagerBot: TManagerBot;
    TradingPlatform: TTradingPlatform;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.DateUtils;

{$R *.fmx}

{ TMainForm }

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  // Запускаем сканирование рынка bybit
  if not TradingPlatform.IsActive then
  begin
    IndexTrade := 0;

    TradingPlatform.Symbol := 'ETHUSDT';
    TradingPlatform.StateMarket.Qty := 0.2;
    TradingPlatform.Start;

    Timer.Enabled := True;

  end;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  if TradingPlatform.IsActive then
    TradingPlatform.Stop;
  Timer.Enabled := False;
end;

constructor TMainForm.Create(AOwner: TComponent);

  procedure _InitManagerBot;
  var
    xBot: TBot;
  begin
    for var i := 0 to 0 do
    begin
      xBot := ManagerBot.AddBot;
      with xBot.TradeBox do
      begin
        OpenLong   := 50;// 50 + (10 - Random(20));
        CloseLong  := 80;// OpenLong + (20 + Random(10));
        OpenShort  := 50;// OpenLong - Random(10);
        CloseShort := 20;// OpenShort - (10 + Random(20));
      end;
    end;
  end;

  procedure SetAddColumn(const AHeader: String);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(nil);
    xCol.Parent := StrGrid;
    xCol.Header := AHeader;
  end;

begin
  inherited Create(AOwner);

  ManagerBot := TManagerBot.Create;

  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;

  TPlatfomBybit(TradingPlatform).ApiKey := '3bvDxJnKzjkIg8y0RV';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'YtpORO6EYWTESXWwCyLiOBm75c1Tv6GSOzqJ';


  ManagerBot.TradingPlatform := TradingPlatform;

  _InitManagerBot;

  SetAddColumn('id');
  SetAddColumn('open_long');
  SetAddColumn('close_long');
  SetAddColumn('open_short');
  SetAddColumn('close_short');

  SetAddColumn('PosCount');
  SetAddColumn('info.qty');
  SetAddColumn('info.profit');
  SetAddColumn('curr.profit');
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(TradingPlatform);
  FreeAndNil(ManagerBot);
  inherited;
end;


procedure TMainForm.TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);

  function _ToBool(const AValue: Boolean): String;
  begin
    case AValue of
      True: Result := 'V';
      False: Result := 'X';
    end;
  end;

var
  xBot: TBot;
  xInfo: TInfoPositionTrading;
  xS: String;
begin
  MemoInfo.BeginUpdate;
  try
    MemoInfo.Lines.Clear;

    Inc(IndexTrade);
    MemoInfo.Lines.Add('IndexTrade: ' + IndexTrade.ToString);
    MemoInfo.Lines.Add('****************************************************');

    MemoInfo.Lines.Add(
      'Ask: ' + TradingPlatform.StateMarket.Ask.ToString + '; ' +
      'Bid: ' + TradingPlatform.StateMarket.Bid.ToString
    );
    MemoInfo.Lines.Add('****************************************************');

    for var i := 0 to AStateMarket.Candels.Count - 1 do
    begin
      var xC := AStateMarket.Candels[i];
      xS :=
        DateTimeToStr(UnixToDateTime(xC.Time)) + '; ' +
        xC.Time.ToString + '; ' +
        xC.Open.ToString + '; ' +
        xC.High.ToString + '; ' +
        xC.Low.ToString + '; ' +
        xC.Close.ToString + ';  ' +
        xC.Vol.ToString;
      MemoInfo.Lines.Add(xS);

      if i > 10 then
      begin
        MemoInfo.Lines.Add('.....');
        Break;
      end;

    end;

    MemoInfo.Lines.Add('****************************************************');
    xS :=
       'ValueRSI: ' + TradingPlatform.ValueRSI.ToString + ' ' +
       'ValueATR: ' + TradingPlatform.ValueATR.ToString + ' ';
    MemoInfo.Lines.Add(xS);

    ManagerBot.SetSelected;
    SetShowGrid;

    if ManagerBot.Items.Count > 0 then
    begin
      MemoInfo.Lines.Add('****************************************************');
      xBot := ManagerBot.Items[0];


      MemoInfo.Lines.Add('  :: >> qty = ' + xInfo.Qty.ToString);
      MemoInfo.Lines.Add('  :: >> profit = ' + xInfo.Profit.ToString);

    end;
  finally
    MemoInfo.EndUpdate;
  end;
end;

procedure TMainForm.SetShowGrid;
var
  xPath: String;
  xBot: TBot;
  i, iCount: Integer;
  xInfo: TInfoPositionTrading;
begin
  xPath := 'd:\work\git\delphi\sample\bot_rsi\bq_rsi_2\app\bin\data\';
  iCount := ManagerBot.Items.Count;
  StrGrid.RowCount := iCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xBot := ManagerBot.Items[i];
      StrGrid.Cells[0,i] := i.ToString;
      StrGrid.Cells[1,i] := xBot.TradeBox.OpenLong.ToString;
      StrGrid.Cells[2,i] := xBot.TradeBox.CloseLong.ToString;
      StrGrid.Cells[3,i] := xBot.TradeBox.OpenShort.ToString;
      StrGrid.Cells[4,i] := xBot.TradeBox.CloseShort.ToString;



//      StrGrid.Cells[3,i] := xBot.Trading.ProfitClosePosition.ToString;
//      StrGrid.Cells[4,i] := xBot.CrossTrading.ProfitClosePosition.ToString;

      StrGrid.Cells[5,i] := xBot.Trading.Positions.Count.ToString;

      StrGrid.Cells[6,i] := xInfo.Qty.ToString;
      StrGrid.Cells[7,i] := xInfo.Profit.ToString;


//      StrGrid.Cells[8,i] := (xInfo.Profit + xBot.CrossTrading.ProfitClosePosition).ToString;


      xBot.Trading.SaveTrading(
        xPath + 'bot_position_' + i.ToString + '.txt'
      );

//      xBot.CrossTrading.SaveTrading(
//        xPath + 'bot_cross_position_' + i.ToString + '.txt'
//      );

    end;
end;

procedure TMainForm.ButtonBuy2Click(Sender: TObject);
begin
  TradingPlatform.SendTrade(
    Time,
    TradingPlatform.StateMarket.Ask,
    0.02,
    TTypeBuySell.tsBuy
  );
end;

procedure TMainForm.ButtonBuyClick(Sender: TObject);
begin
  TradingPlatform.SendTrade(
    Time,
    TradingPlatform.StateMarket.Ask,
    0.01,
    TTypeBuySell.tsBuy
  );
end;

procedure TMainForm.ButtonSell2Click(Sender: TObject);
begin
  TradingPlatform.SendTrade(
    Time,
    TradingPlatform.StateMarket.Bid,
    0.02,
    TTypeBuySell.tsSell
  );
end;

procedure TMainForm.ButtonSellClick(Sender: TObject);
begin
  TradingPlatform.SendTrade(
    Time,
    TradingPlatform.StateMarket.Bid,
    0.01,
    TTypeBuySell.tsSell
  );
end;

var
  localValueRSI: Double = 0;
  localStepValue: Double = 5;
  localTrade: Integer = 0;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
//  case localTrade of
//    0: begin
//      localValueRSI := localValueRSI + localStepValue;
//      if localValueRSI >= 60 then
//      begin
//        localValueRSI := 60;
//        localTrade := 1;
//      end;
//    end;
//    1: begin
//      localValueRSI := localValueRSI - localStepValue;
//      if localValueRSI <= 40 then
//      begin
//        localValueRSI := 40;
//        localTrade := 0;
//      end;
//    end;
//  end;
//
//  for var xBot in ManagerBot.Items do
//    xBot.SetSelected(localValueRSI);
end;


end.
