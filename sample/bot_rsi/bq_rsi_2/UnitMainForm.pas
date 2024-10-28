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
  UniCategoryListFrame;

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
    MemoBot: TMemo;
    MemoReversBot: TMemo;
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
  private
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
    procedure BotOnSendTrade(ASender: TObject; const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell);
    procedure ReversBotOnSendTrade(ASender: TObject; const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell);
  public
    Bot: TBot;
    ReversBot: TBot;
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
begin
  inherited Create(AOwner);

  Bot := TBot.Create;
  Bot.OnSendTrade := BotOnSendTrade;

  ReversBot := TBot.Create;
  ReversBot.OnSendTrade := ReversBotOnSendTrade;
  ReversBot.TypeBot := TTypeBot.tbReverse;

  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;

  Bot.TradingPlatform := TradingPlatform;
  ReversBot.TradingPlatform := TradingPlatform;

  InitFrame;

  FCategoryListSell.ManagerCategory := Bot.ManagerCategorySell;
  FCategoryListBuy.ManagerCategory := Bot.ManagerCategoryBuy;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(TradingPlatform);
  FreeAndNil(Bot);
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

  procedure _VirtualTrading;
  var
    xPosition: TBufferTrading.TPosition;
    xTrading: TBufferTrading;
    i, iCount: Integer;
  var
    xProfit: Double;
    xTrade: TBufferTrading.TTrade;
    j, jCount: Integer;
  begin
    xTrading := Bot.Trading;
    MemoBot.Lines.Clear;
    MemoBot.Lines.Add(
      'Profit: ' + xTrading.ProfitClosePosition.ToString
    );

    iCount := xTrading.Positions.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xPosition := xTrading.Positions[i];

        xProfit := 0;
        case xPosition.Side of
          tsBuy : xProfit := xPosition.GetProfit(TradingPlatform.StateMarket.Bid);
          tsSell: xProfit := xPosition.GetProfit(TradingPlatform.StateMarket.Ask);
        end;
        MemoBot.Lines.Add(
          'pos: ' + xPosition.ToString + ' res: ' +
          '; Profit: ' + xProfit.ToString +
          '; Qty: ' + xPosition.Qty.ToString
        );

        jCount := xPosition.History.Count;
        if jCount > 0 then
          for j := 0 to jCount - 1 do
          begin
            xTrade := xPosition.History[j];

            MemoBot.Lines.Add(
              '   >>' +
              xTrade.Time.ToString + ';' +
              xTrade.Price.ToString + ';' +
              xTrade.Qty.ToString + ';' +
              GetStrToSide(xTrade.Side) + ';'
            );

          end;

      end;
  end;

  procedure _CrossVirtualTrading;
  var
    xPosition: TBufferTrading.TPosition;
    xTrading: TBufferTrading;
    i, iCount: Integer;
  var
    xProfit: Double;
    xTrade: TBufferTrading.TTrade;
    j, jCount: Integer;
  begin
    xTrading := ReversBot.Trading;
    MemoReversBot.Lines.Clear;
    MemoReversBot.Lines.Add(
      'Profit: ' + xTrading.ProfitClosePosition.ToString
    );

    iCount := xTrading.Positions.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xPosition := xTrading.Positions[i];

        xProfit := 0;
        case xPosition.Side of
          tsBuy : xProfit := xPosition.GetProfit(TradingPlatform.StateMarket.Bid);
          tsSell: xProfit := xPosition.GetProfit(TradingPlatform.StateMarket.Ask);
        end;
        MemoReversBot.Lines.Add(
          'pos: ' + xPosition.ToString + ' res: ' +
          '; Profit: ' + xProfit.ToString +
          '; Qty: ' + xPosition.Qty.ToString
        );

        jCount := xPosition.History.Count;
        if jCount > 0 then
          for j := 0 to jCount - 1 do
          begin
            xTrade := xPosition.History[j];

            MemoReversBot.Lines.Add(
              '   >>' +
              xTrade.Time.ToString + ';' +
              xTrade.Price.ToString + ';' +
              xTrade.Qty.ToString + ';' +
              GetStrToSide(xTrade.Side) + ';'
            );

          end;

      end;
  end;


var
  xS: String;
begin
  MemoInfo.BeginUpdate;
  try
    MemoInfo.Lines.Clear;
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
       'ValueRSI: ' + Bot.ValueRSI.ToString + ' ' +
       'ValueATR: ' + Bot.ValueATR.ToString + ' ' +
       'StopLossPrice: ' + Bot.StopLossPrice.ToString;
    MemoInfo.Lines.Add(xS);

    MemoInfo.Lines.Add('****************************************************');
    MemoInfo.Lines.Add('Продажа');
    for var i := Bot.ManagerCategorySell.Count - 1 downto 0 do
    begin
      var xCategory := Bot.ManagerCategorySell.Items[i];
      xS := 'Ind [' + i.ToString + '] ' +
        '[' + _ToBool(xCategory.IsActive) + '] ' +
        'Active: ' + xCategory.ActiveLevel.Value.ToString + '; ' +
        '[' + _ToBool(xCategory.IsReActive) + '] ' +
        'ReActive: ' + xCategory.ReActiveLevel.Value.ToString + '; ' +
        'Qty: ' + xCategory.Qty.ToString;
      MemoInfo.Lines.Add(xS);
    end;

    MemoInfo.Lines.Add('****************************************************');
    MemoInfo.Lines.Add('Покупка');
    for var i := 0 to Bot.ManagerCategoryBuy.Count - 1 do
    begin
      var xCategory := Bot.ManagerCategoryBuy.Items[i];
      xS := 'Ind [' + i.ToString + '] ' +
        '[' + _ToBool(xCategory.IsActive) + '] ' +
        'Active: ' + xCategory.ActiveLevel.Value.ToString + '; ' +
        '[' + _ToBool(xCategory.IsReActive) + '] ' +
        'ReActive: ' + xCategory.ReActiveLevel.Value.ToString + '; ' +
        'Qty: ' + xCategory.Qty.ToString;
      MemoInfo.Lines.Add(xS);
    end;

    // ------------------------------------------------------------------------
    _VirtualTrading;
    _CrossVirtualTrading;
  finally
    MemoInfo.EndUpdate;
  end;
  Bot.SetSelected;
  ReversBot.SetSelected;
end;

procedure TMainForm.BotOnSendTrade(ASender: TObject; const ATime: TDateTime;
  const APrice, AQty: Double; ASide: TTypeBuySell);
begin

end;

procedure TMainForm.ReversBotOnSendTrade(ASender: TObject;
  const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell);
begin

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
