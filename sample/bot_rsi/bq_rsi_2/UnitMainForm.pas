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
  UniCategoryListFrame, System.Rtti, FMX.Grid.Style, FMX.Grid;

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
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
    procedure ButtonBuy2Click(Sender: TObject);
    procedure ButtonSell2Click(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    FCategoryListSell: TCategoryListFrame;
    FCategoryListBuy: TCategoryListFrame;
    procedure InitFrame;
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
  if not TradingPlatform.IsActive then
  begin
    IndexTrade := 0;

    TradingPlatform.Symbel := 'ETHUSDT';
    TradingPlatform.StateMarket.Qty := 0.2;
    TradingPlatform.Start;
  end;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  if TradingPlatform.IsActive then
    TradingPlatform.Stop;
end;

constructor TMainForm.Create(AOwner: TComponent);

  procedure _InitManagerBot;
  var
    xBot: TBot;
  begin
    var xStep := 0.5;
    for var i := 0 to 9 do
    begin
      xBot := ManagerBot.AddBot;
      xBot.TypeBot := TTypeBot.tbLong;
      xBot.ValueCof := 0.5 + xStep * i;
    end;
    for var i := 0 to 9 do
    begin
      xBot := ManagerBot.AddBot;
      xBot.TypeBot := TTypeBot.tbShort;
      xBot.ValueCof := 0.5 + xStep * i;
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

var
  xBot: TBot;
begin
  inherited Create(AOwner);

  ManagerBot := TManagerBot.Create;

  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;

  ManagerBot.TradingPlatform := TradingPlatform;

  _InitManagerBot;
  InitFrame;

  xBot := ManagerBot.Items[0];
  FCategoryListSell.ManagerCategory := xBot.ManagerCategorySell;
  FCategoryListBuy.ManagerCategory := xBot.ManagerCategoryBuy;


  SetAddColumn('id');
  SetAddColumn('strateg');
  SetAddColumn('êîôô');
  SetAddColumn('profit');
  SetAddColumn('cross.profit');
  SetAddColumn('PosCount');

end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(TradingPlatform);
  FreeAndNil(ManagerBot);
  inherited;
end;

procedure TMainForm.InitFrame;
begin
  FCategoryListSell:= TCategoryListFrame.Create(nil);
  FCategoryListSell.Parent := LayoutSell;
  FCategoryListSell.Align := TAlignLayout.Client;

  FCategoryListBuy := TCategoryListFrame.Create(nil);
  FCategoryListBuy.Parent := LayoutBuy;
  FCategoryListBuy.Align := TAlignLayout.Client;
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
       'ValueRSI: ' + TradingPlatform.ValueRSI.ToString + ' ' +
       'ValueATR: ' + TradingPlatform.ValueATR.ToString + ' ';
    MemoInfo.Lines.Add(xS);

    ManagerBot.SetSelected;
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

      StrGrid.Cells[0,i] := i.ToString;
      StrGrid.Cells[1,i] := GetStrToTypeBot(xBot.TypeBot);
      StrGrid.Cells[2,i] := xBot.ValueCof.ToString;
      StrGrid.Cells[3,i] := xBot.Trading.ProfitClosePosition.ToString;
      StrGrid.Cells[4,i] := xBot.CrossTrading.ProfitClosePosition.ToString;
      StrGrid.Cells[5,i] := xBot.Trading.Positions.Count.ToString;

      xBot.Trading.SaveTrading(
        xPath + 'bot_position_' + i.ToString + '.txt'
      );

      xBot.CrossTrading.SaveTrading(
        xPath + 'bot_cross_position_' + i.ToString + '.txt'
      );

    end;
end;

procedure TMainForm.ButtonBuy2Click(Sender: TObject);
begin
  TradingPlatform.SendTrade(
    Time,
    TradingPlatform.StateMarket.Ask,
    2,
    TTypeBuySell.tsBuy
  );
end;

procedure TMainForm.ButtonBuyClick(Sender: TObject);
begin
  TradingPlatform.SendTrade(
    Time,
    TradingPlatform.StateMarket.Ask,
    1,
    TTypeBuySell.tsBuy
  );
end;

procedure TMainForm.ButtonSell2Click(Sender: TObject);
begin
  TradingPlatform.SendTrade(
    Time,
    TradingPlatform.StateMarket.Bid,
    2,
    TTypeBuySell.tsSell
  );
end;

procedure TMainForm.ButtonSellClick(Sender: TObject);
begin
  TradingPlatform.SendTrade(
    Time,
    TradingPlatform.StateMarket.Bid,
    1,
    TTypeBuySell.tsSell
  );
end;

end.
