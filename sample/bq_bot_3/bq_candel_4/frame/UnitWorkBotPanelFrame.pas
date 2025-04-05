unit UnitWorkBotPanelFrame;

interface

{$I debug_volt.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Rtti,

  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.Objects,
  FMX.Layouts,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Grid,

  UnitPositionGridFrame,

  Lb.Bot.Candel,
  Lb.Breakdown,

  Lb.SysUtils,
  Lb.Bybit.SysUtils,
  Lb.Platform,
  Lb.Platform.Bybit,
  Lb.Journal.Trading, FMX.ListBox;

type
  ///<summary>
  /// Фрейм работы робота - объекта
  ///</summary>
  TWorkBotPanelFrame = class(TFrame)
    LayoutStatusMarket: TLayout;
    RectangleStatusMarket: TRectangle;
    EditNewCandel: TEdit;
    EditUpDataCandel: TEdit;
    EditProfit: TEdit;
    EditProfitFeeRatesTaker: TEdit;
    EditProfitFeeRatesMaker: TEdit;
    LayoutPositionGrid: TLayout;
    ListBoxWorkBot: TListBox;
    procedure ListBoxWorkBotClick(Sender: TObject);
  private
    FMainFormLog: IMainFormLog;
  private
    FSelectedWorkBot: TWorkBot;
    FTradingPlatform: TTradingPlatform;
    procedure SetTradingPlatform(const Value: TTradingPlatform);
  protected
    FCountNewCandel: Integer;
    FCountUpDataCandel: Integer;
  public

    WorkBots: TWorkBotList;
    PositionGridFrame: TPositionGridFrame;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure TradingPlatformNewCandel;
    procedure TradingPlatformStateMarket(AStateMarket: TStateMarket);

    property MainFormLog: IMainFormLog write FMainFormLog;
    property SelectedWorkBot: TWorkBot read FSelectedWorkBot write FSelectedWorkBot;
    property TradingPlatform: TTradingPlatform read FTradingPlatform write SetTradingPlatform;
  public
    procedure SetLog(S: String);
  end;

implementation

{$R *.fmx}

uses
  Lb.Logger;

{ TWorkBotPanelFrame }

constructor TWorkBotPanelFrame.Create(AOwner: TComponent);

  procedure _SetInitilizationWorkBots;
  var
    i: Integer;
    xWorkBot: TWorkBot;
  begin
    WorkBots.Clear;
    ListBoxWorkBot.Items.Clear;

    for i := 1 to 20 do
    begin
      xWorkBot := TWorkBot.Create;
      xWorkBot.CloseTriling := 0.5 * i;
      WorkBots.Add(xWorkBot);

      ListBoxWorkBot.Items.Add('triling_' + xWorkBot.CloseTriling.ToString);

      FSelectedWorkBot := xWorkBot;
    end;

  end;

begin
  inherited;

  FCountNewCandel := 0;
  FCountUpDataCandel := 0;

  PositionGridFrame := TPositionGridFrame.Create(nil);
  PositionGridFrame.Parent := LayoutPositionGrid;
  PositionGridFrame.Align := TAlignLayout.Client;

  FMainFormLog := nil;

  FSelectedWorkBot := nil;
  WorkBots := TWorkBotList.Create;
  _SetInitilizationWorkBots;

end;

destructor TWorkBotPanelFrame.Destroy;
begin
  FreeAndNil(WorkBots);
  FreeAndNil(PositionGridFrame);
  inherited;
end;

procedure TWorkBotPanelFrame.ListBoxWorkBotClick(Sender: TObject);
var
  xIndex: Integer;
begin
  FSelectedWorkBot := nil;
  xIndex := ListBoxWorkBot.ItemIndex;
  if xIndex >= 0 then
  begin
    FSelectedWorkBot := WorkBots[xIndex];
  end;

  if Assigned(FSelectedWorkBot) then
  begin
    PositionGridFrame.UpDataJournalManager(FSelectedWorkBot.JournalManager);
    EditProfit.Text := FSelectedWorkBot.JournalManager.Profit.ToString;
    EditProfitFeeRatesTaker.Text := FSelectedWorkBot.JournalManager.ProfitFeeRatesTaker.ToString;
    EditProfitFeeRatesMaker.Text := FSelectedWorkBot.JournalManager.ProfitFeeRatesMaker.ToString;
  end;

end;

procedure TWorkBotPanelFrame.TradingPlatformNewCandel;

  procedure _SetTradingNewCandel;
  begin
    if WorkBots.Count > 0 then
      for var xWorkBot in WorkBots do
        xWorkBot.SetTradingNewCandel;
  end;

begin
  Inc(FCountNewCandel);
  FCountUpDataCandel := 0;

  EditNewCandel.Text := FCountNewCandel.ToString;
  EditUpDataCandel.Text := FCountUpDataCandel.ToString;

  if Assigned(FTradingPlatform) then
    _SetTradingNewCandel;

end;

procedure TWorkBotPanelFrame.TradingPlatformStateMarket(AStateMarket: TStateMarket);

  procedure _SetUpDataPosition(AJournalManager: TJournalManager);
  var
    i, iCount: Integer;
    xPosition: TJournalPosition;
  begin
    iCount := AJournalManager.Positions.Count;
    if iCount > 0 then
      for i := iCount - 1 downto 0 do
      begin
        xPosition := AJournalManager.Positions[i];
        if xPosition.TypeTrade = TTypeTrade.ttOpen then
        begin
          case xPosition.Side of
            TTypeBuySell.tsBuy: xPosition.SetUpData(FTradingPlatform.StateMarket.Bid);
            TTypeBuySell.tsSell: xPosition.SetUpData(FTradingPlatform.StateMarket.Ask);
          end;
        end;
      end;
  end;

  procedure _TradingPlatformStateMarket;
  begin
    // Обновление текущий позиции по инструменту
    if WorkBots.Count > 0 then
      for var xWorkBot in WorkBots do
      begin
        xWorkBot.SetTradingPlatform(FTradingPlatform);
        _SetUpDataPosition(xWorkBot.JournalManager);
      end;
  end;

begin
{$IFDEF DBG_STATE_MARKET}
  TLogger.LogTree(0,'TWorkBotPanelFrame.TradingPlatformStateMarket: Состояние рынка');
{$ENDIF}

  Inc(FCountUpDataCandel);
  EditNewCandel.Text := FCountNewCandel.ToString;
  EditUpDataCandel.Text := FCountUpDataCandel.ToString;

  if Assigned(FTradingPlatform) then
    _TradingPlatformStateMarket;

  if Assigned(FSelectedWorkBot) then
  begin
    PositionGridFrame.UpDataJournalManager(FSelectedWorkBot.JournalManager);
    EditProfit.Text := FSelectedWorkBot.JournalManager.Profit.ToString;
    EditProfitFeeRatesTaker.Text := FSelectedWorkBot.JournalManager.ProfitFeeRatesTaker.ToString;
    EditProfitFeeRatesMaker.Text := FSelectedWorkBot.JournalManager.ProfitFeeRatesMaker.ToString;
  end;
end;

procedure TWorkBotPanelFrame.SetLog(S: String);
begin
  if Assigned(FMainFormLog) then
    FMainFormLog.LogMsg(S);
end;

procedure TWorkBotPanelFrame.SetTradingPlatform(const Value: TTradingPlatform);

  procedure _SetTradingPlatform;
  var
    i: Integer;
    xWorkBot: TWorkBot;
  begin
    if WorkBots.Count > 0 then
      for xWorkBot in WorkBots do
        xWorkBot.SetTradingPlatform(FTradingPlatform);
  end;


begin
  FTradingPlatform := Value;
  _SetTradingPlatform;
end;

end.
