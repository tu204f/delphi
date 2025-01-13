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

  Lb.Journal.Trading.V2,

  Lb.SysUtils,
  Lb.Bot,
  Lb.Platform,
  Lb.Platform.Bybit,
  Lb.TradeBox,
  FMX.TabControl,
  FMX.Layouts,

  System.Rtti,
  FMX.Grid.Style,
  FMX.Grid,
  FMX.Objects,

  UnitJournalPositionFrame;

type
  TMainForm = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    TabControl: TTabControl;
    TabItemLog: TTabItem;
    MemoInfo: TMemo;
    StrGrid: TStringGrid;
    TextSumProfit: TText;
    Timer: TTimer;
    TextStatus: TText;
    Button1: TButton;
    TabItemPosition: TTabItem;
    LayoutJournalPosition: TLayout;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    procedure SetShowGrid;
  private
    JournalPositionFrame: TJournalPositionFrame;
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


constructor TMainForm.Create(AOwner: TComponent);

  procedure _InitManagerBot;
  var
    xBot: TBot;
  begin
    for var i := 0 to 0 do
    begin
      xBot := ManagerBot.AddBot;
      xBot.SetTradeBox(50,80,50,20);
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

  SetAddColumn('[1]id');
  SetAddColumn('[2]open_long');
  SetAddColumn('[3]close_long');
  SetAddColumn('[4]open_short');
  SetAddColumn('[5]close_short');
  SetAddColumn('[6]PosCount');
  SetAddColumn('[7]info.side');
  SetAddColumn('[8]info.qty');
  SetAddColumn('[9]info.profit');
  SetAddColumn('[10]max_profit');
  SetAddColumn('[11]min_profit');
  SetAddColumn('[12]sum_profit');

  JournalPositionFrame := TJournalPositionFrame.Create(nil);
  JournalPositionFrame.Parent := LayoutJournalPosition;
  JournalPositionFrame.Align := TAlignLayout.Client;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(JournalPositionFrame);
  FreeAndNil(TradingPlatform);
  FreeAndNil(ManagerBot);
  inherited;
end;

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

    TextStatus.Text := 'Активно';
  end;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  if TradingPlatform.IsActive then
    TradingPlatform.Stop;
  Timer.Enabled := False;
  TextStatus.Text := 'Остановка';
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
       'ValueRSI: ' + TradingPlatform.ValueRSI.ToString + '; ' +
       'ValueAveragRSI: ' + TradingPlatform.ValueAveragRSI.ToString + '; ' +
       'ValueATR: ' + TradingPlatform.ValueATR.ToString + ';';
    MemoInfo.Lines.Add(xS);

    SetShowGrid;

  finally
    MemoInfo.EndUpdate;
  end;
end;

procedure TMainForm.SetShowGrid;
var
  xPath: String;
  xBot: TBot;
  i, iCount: Integer;
begin
  xPath := 'd:\work\git\delphi\sample\bot_rsi\bq_rsi_2\app\bin\data\';
  iCount := ManagerBot.Items.Count;
  StrGrid.RowCount := iCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xBot := ManagerBot.Items[i];

      (*
        SetAddColumn('[1]id');
        SetAddColumn('[2]open_long');
        SetAddColumn('[3]close_long');
        SetAddColumn('[4]open_short');
        SetAddColumn('[5]close_short');
        SetAddColumn('[6]PosCount');
        SetAddColumn('[7]info.side');
        SetAddColumn('[8]info.qty');
        SetAddColumn('[9]info.profit');
        SetAddColumn('[10]max_profit');
        SetAddColumn('[11]min_profit');
        SetAddColumn('[12]sum_profit');
      *)

      StrGrid.Cells[0,i] := i.ToString;
      StrGrid.Cells[1,i] := xBot.TradeBox.OpenLong.ToString;
      StrGrid.Cells[2,i] := xBot.TradeBox.CloseLong.ToString;
      StrGrid.Cells[3,i] := xBot.TradeBox.OpenShort.ToString;
      StrGrid.Cells[4,i] := xBot.TradeBox.CloseShort.ToString;


      if xBot.Manager.IsCurrentPosition then
      begin
        StrGrid.Cells[5,i] := xBot.Manager.CurrentPosition.Trades.Count.ToString;
        StrGrid.Cells[6,i] := GetStrToSide(xBot.Manager.CurrentPosition.Side);
        StrGrid.Cells[7,i] := xBot.Manager.CurrentPosition.Qty.ToString;

        var xLast := xBot.TradingPlatform.StateMarket.Last;

        xBot.Manager.CurrentPosition.SetUpDateValue(
          xLast,
          xBot.TradingPlatform.ValueRSI,
          xBot.TradingPlatform.ValueAveragRSI,
          xBot.TradingPlatform.ValueATR
        );

        StrGrid.Cells[8,i] := xBot.Manager.CurrentPosition.Profit.ToString;
        StrGrid.Cells[9,i] := xBot.Manager.CurrentPosition.MaxProfit.ToString;
        StrGrid.Cells[10,i] := xBot.Manager.CurrentPosition.MinProfit.ToString;
        StrGrid.Cells[11,i] := xBot.Manager.Profit.ToString;
      end
      else
      begin
        StrGrid.Cells[5,i] := '';
        StrGrid.Cells[6,i] := '';
        StrGrid.Cells[6,i] := '';
        StrGrid.Cells[7,i] := '';
        StrGrid.Cells[8,i] := '';
        StrGrid.Cells[9,i] := '';
        StrGrid.Cells[10,i] := '';
        StrGrid.Cells[11,i] := '';
      end;

      // Показывать текушие стояние бота
      JournalPositionFrame.SetDateJournalPosition(xBot.Manager.CurrentPosition);
    end;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  // Перебираем списко ботов для работы
  //for var xBot in ManagerBot.Items do
  //  xBot.SetSelected;
end;


end.
