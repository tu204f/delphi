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

  Lb.Platform.Trading,
  Lb.SysUtils,
  Lb.Bot,
  Lb.Platform,
  Lb.Platform.Bybit,
  Lb.Criteria,
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
    Memo1: TMemo;
    GridLayout: TGridPanelLayout;
    LayoutSell: TLayout;
    LayoutBuy: TLayout;
    MemoSendTrade: TMemo;
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
  public
    Bot: TBot;
    TradingPlatform: TTradingPlatform;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

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

  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;
  Bot.TradingPlatform := TradingPlatform;

  InitFrame;

  FCategoryListSell.ManagerCriteria := Bot.ManagerCriteriaSell;
  FCategoryListBuy.ManagerCriteria := Bot.ManagerCriteriaBuy;
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
    xPosition: TPlatformTrading.TPosition;
    xTrading: TPlatformTrading;
    i, iCount: Integer;
  var
    xTrade: TPlatformTrading.TTrade;
    j, jCount: Integer;
  begin
    xTrading := TradingPlatform.Trading;
    iCount := xTrading.Positions.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xPosition := xTrading.Positions[i];
        Memo1.Lines.Add('pos: ' + xPosition.ToString);

        jCount := xPosition.History.Count;
        if jCount > 0 then
          for j := 0 to jCount - 1 do
          begin
            xTrade := xPosition.History[j];

            Memo1.Lines.Add(
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
  Memo1.BeginUpdate;
  try
    Memo1.Lines.Clear;
    Memo1.Lines.Add(
      'Ask: ' + TradingPlatform.StateMarket.Ask.ToString + '; ' +
      'Bid: ' + TradingPlatform.StateMarket.Bid.ToString
    );
    Memo1.Lines.Add('****************************************************');

    for var i := 0 to AStateMarket.Candels.Count - 1 do
    begin
      var xC := AStateMarket.Candels[i];
      xS :=
        xC.Time.ToString + '; ' +
        xC.Open.ToString + '; ' +
        xC.High.ToString + '; ' +
        xC.Low.ToString + '; ' +
        xC.Close.ToString + ';  ' +
        xC.Vol.ToString;
      Memo1.Lines.Add(xS);

      if i > 10 then
      begin
        Memo1.Lines.Add('.....');
        Break;
      end;

    end;

    Memo1.Lines.Add('****************************************************');
    xS := 'ValueRSI: ' + Bot.ValueRSI.ToString;
    Memo1.Lines.Add(xS);

    Memo1.Lines.Add('****************************************************');
    Memo1.Lines.Add('Продажа');
    for var i := Bot.ManagerCriteriaSell.Count - 1 downto 0 do
    begin
      var xCriteria := Bot.ManagerCriteriaSell.Items[i];
      xS := 'Ind [' + i.ToString + '] ' +
        '[' + _ToBool(xCriteria.IsActive) + '] ' +
        'Active: ' + xCriteria.ActiveLevel.Value.ToString + '; ' +
        '[' + _ToBool(xCriteria.IsReActive) + '] ' +
        'ReActive: ' + xCriteria.ReActiveLevel.Value.ToString + '; ' +
        'Qty: ' + xCriteria.Qty.ToString;
      Memo1.Lines.Add(xS);
    end;

    Memo1.Lines.Add('****************************************************');
    Memo1.Lines.Add('Покупка');
    for var i := 0 to Bot.ManagerCriteriaBuy.Count - 1 do
    begin
      var xCriteria := Bot.ManagerCriteriaBuy.Items[i];
      xS := 'Ind [' + i.ToString + '] ' +
        '[' + _ToBool(xCriteria.IsActive) + '] ' +
        'Active: ' + xCriteria.ActiveLevel.Value.ToString + '; ' +
        '[' + _ToBool(xCriteria.IsReActive) + '] ' +
        'ReActive: ' + xCriteria.ReActiveLevel.Value.ToString + '; ' +
        'Qty: ' + xCriteria.Qty.ToString;
      Memo1.Lines.Add(xS);
    end;

    Memo1.Lines.Add('****************************************************');
    Memo1.Lines.Add('История - операций');
    _VirtualTrading;

  finally
    Memo1.EndUpdate;
  end;
  Bot.SetSelected;
end;

procedure TMainForm.BotOnSendTrade(ASender: TObject; const ATime: TDateTime;
  const APrice, AQty: Double; ASide: TTypeBuySell);
begin
  with MemoSendTrade.Lines do
  begin
    Add('BotOnSendTrade:');
    Add(' >> ' + DateTimeToStr(ATime));
    Add(' >> ' + AQty.ToString);
    Add(' >> ' + GetStrToSide(ASide));
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
