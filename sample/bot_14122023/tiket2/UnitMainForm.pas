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
  FMX.Dialogs, FMX.Memo.Types, FMX.StdCtrls, FMX.Objects, FMXTee.Engine,
  FMXTee.Series, FMXTee.Procs, FMXTee.Chart, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo,
  Lb.SysUtils,
  Lb.ReadPrice,
  Lb.Bot.Tiket,
  FMX.Layouts, System.Rtti, FMX.Grid.Style, FMX.Grid;

type
  TMainForm = class(TForm)
    Memo1: TMemo;
    Chart1: TChart;
    Series1: TLineSeries;
    TextStatus: TText;
    ButtonRead: TButton;
    TimerRead: TTimer;
    Memo2: TMemo;
    CheckBox1: TCheckBox;
    StrGrid: TStringGrid;
    Text1: TText;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerReadTimer(Sender: TObject);
    procedure ButtonReadClick(Sender: TObject);
  private
    IndexTiket: Integer;
  protected
    TradeBot: TTradeBot;
    ObverseID: Integer;
    ReverseID: Integer;
    procedure ShowTradeBot;
  protected
    Bot: TPairBot;
    procedure BotObverseOpenPosition(const ASander: TObject; AMode: TMode; APrice: Double);
    procedure BotObverseClosePosition(const ASander: TObject; AMode: TMode; APrice: Double);
    procedure BotReverseOpenPosition(const ASander: TObject; AMode: TMode; APrice: Double);
    procedure BotReverseClosePosition(const ASander: TObject; AMode: TMode; APrice: Double);
  public
    BeginTime: Int64;
    SourceTikets: TBybitSourceTikets;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);

  procedure _SetAddCol(const AHeader: String);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(StrGrid);
    xCol.Header := AHeader;
    xCol.Parent := StrGrid;
  end;

var
  xPath: String;
begin
  Bot := TPairBot.Create;
  Bot.StopLoss := -20;
  Bot.CountStop := 3;
  Bot.TakeProfit := 100;

  Bot.Obverse.OnOpenPosition := BotObverseOpenPosition;
  Bot.Obverse.OnClosePosition := BotObverseClosePosition;
  Bot.Reverse.OnOpenPosition := BotReverseOpenPosition;
  Bot.Reverse.OnClosePosition := BotReverseClosePosition;

  xPath := 'd:\work\git\delphi\sample\bybit\sample\recent_trade\bin\trad_2.txt';
  SourceTikets := TBybitSourceTikets.Create(xPath);

  _SetAddCol('ID');
  _SetAddCol('OpenTime');
  _SetAddCol('CloseTime');
  _SetAddCol('Open');
  _SetAddCol('Close');
  _SetAddCol('Mode');
  _SetAddCol('Quantity');
  _SetAddCol('StatusBot');
  _SetAddCol('Status');
  _SetAddCol('Profit');

  TradeBot := TTradeBot.Create;
end;



procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(TradeBot);
  FreeAndNil(SourceTikets);
  FreeAndNil(Bot);
end;

procedure TMainForm.ButtonReadClick(Sender: TObject);
begin

  Bot.Default;

  Series1.Clear;

  SourceTikets.First;

  BeginTime := SourceTikets.Trade.Time;
  IndexTiket := 0;
  TimerRead.Enabled := not TimerRead.Enabled;


  if TimerRead.Enabled then
  begin
    ButtonRead.Text := 'Стоп';
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;
    TradeBot.Positions.Clear;
  end
  else
    ButtonRead.Text := 'Старт';

end;

procedure TMainForm.BotObverseOpenPosition(const ASander: TObject; AMode: TMode; APrice: Double);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('bot ### Obverse_Open');
  Memo1.Lines.Add('open:' + APrice.ToString + ' ' + GetStrToMode(AMode));

  ObverseID := TradeBot.GetOpenPosition(
    Bot.Obverse.OpenTime,
    APrice,
    Bot.Obverse.Mode,
    1,
    TTradeBot.TStatusBot.sbObverse
  );

end;


procedure TMainForm.BotObverseClosePosition(const ASander: TObject; AMode: TMode; APrice: Double);
var
  xProfit: Double;
begin
  Memo1.Lines.Add('bot ### Obverse_Close');
  Memo1.Lines.Add('close:' + APrice.ToString + ' ' + GetStrToMode(AMode));
  Memo1.Lines.Add('HP:' + Bot.Obverse.HealthPoints.ToString);
  //***************************************************************************
  xProfit := 0;
  case AMode of
    tmBuy: xProfit := Bot.Obverse.ClosePrice - Bot.Obverse.OpenPrice;
    tmSell: xProfit := Bot.Obverse.OpenPrice - Bot.Obverse.ClosePrice;
  end;
  Memo1.Lines.Add('Profit' + xProfit.ToString);
  //***************************************************************************
  Series1.AddY(Bot.HealthPoints);

  if not Bot.IsPosition then
  begin
    TradeBot.SetClosePosition(
      ObverseID,
      Bot.Obverse.CloseTime,
      APrice
    );
    ObverseID := -1;
  end;
end;


procedure TMainForm.BotReverseOpenPosition(const ASander: TObject; AMode: TMode; APrice: Double);
begin
  Memo2.Lines.Clear;
  Memo2.Lines.Add('bot ### Reverse_Open');
  Memo2.Lines.Add('open:' + APrice.ToString + ' ' + GetStrToMode(AMode));

  ReverseID := TradeBot.GetOpenPosition(
    Bot.Reverse.OpenTime,
    APrice,
    Bot.Reverse.Mode,
    1,
    TTradeBot.TStatusBot.sbReverse
  );

end;

procedure TMainForm.BotReverseClosePosition(const ASander: TObject; AMode: TMode; APrice: Double);
var
  xProfit: Double;
begin
  Memo2.Lines.Add('bot ### Reverse_Close');
  Memo2.Lines.Add('close:' + APrice.ToString + ' ' + GetStrToMode(AMode));
  Memo2.Lines.Add('HP:' + Bot.Reverse.HealthPoints.ToString);
  //***************************************************************************
  xProfit := 0;
  case AMode of
    tmBuy: xProfit := Bot.Reverse.ClosePrice - Bot.Reverse.OpenPrice;
    tmSell: xProfit := Bot.Reverse.OpenPrice - Bot.Reverse.ClosePrice;
  end;
  Memo2.Lines.Add('Profit' + xProfit.ToString);
  //***************************************************************************
  Series1.AddY(Bot.HealthPoints);

  if not Bot.IsPosition then
  begin
    TradeBot.SetClosePosition(
      ReverseID,
      Bot.Reverse.CloseTime,
      APrice
    );
    ReverseID := -1;
  end;

end;


procedure TMainForm.ShowTradeBot;
var
  i, iCount: Integer;
  xPosition: TTradeBot.TPosition;
begin
  iCount := TradeBot.Positions.Count;
  StrGrid.RowCount := iCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPosition := TradeBot.Positions.Items[iCount - i - 1];
      StrGrid.Cells[0,i] := xPosition.ID.ToString;
      StrGrid.Cells[1,i] := xPosition.OpenTime.ToString;
      StrGrid.Cells[2,i] := xPosition.CloseTime.ToString;
      StrGrid.Cells[3,i] := xPosition.Open.ToString;
      StrGrid.Cells[4,i] := xPosition.Close.ToString;
      StrGrid.Cells[5,i] := GetStrToMode(xPosition.Mode);
      StrGrid.Cells[6,i] := xPosition.Quantity.ToString;
      case xPosition.StatusBot of
        sbObverse: StrGrid.Cells[7,i] := 'obverse';
        sbReverse: StrGrid.Cells[7,i] := 'reverse';
      else
        StrGrid.Cells[7,i] := 'error';
      end;
      case xPosition.Status of
        spOpen : StrGrid.Cells[8,i] := 'open';
        spClose: StrGrid.Cells[8,i] := 'close';
      else
        StrGrid.Cells[8,i] := 'error';
      end;
      StrGrid.Cells[9,i] := xPosition.Profit.ToString;
    end;
end;


procedure TMainForm.TimerReadTimer(Sender: TObject);

  function _Read: Double;
  var
    xTrade: TBybitSourceTikets.TTrade;
    xBeginTime, xEndTime: Int64;
  begin
    Result := 0;
    try
      xBeginTime := BeginTime;
      xEndTime   := xBeginTime + 300000;

      xTrade := SourceTikets.Trade;
      if (xBeginTime >= xTrade.Time) and (xTrade.Time < xEndTime) then
      begin
        Bot.SetUpPosition(xTrade.Time,xTrade.Price);
      end
      else
      begin
        if not Bot.IsPosition then
        begin
          Bot.SetMode;
          Bot.SetOpenPosition(xTrade.Time, xTrade.Price);
        end;
        BeginTime  := xEndTime;
      end;

      Result := xTrade.Price;

      Inc(IndexTiket);
      TextStatus.Text :=
        IndexTiket.ToString + ' :: ' +
        xTrade.Time.ToString;

      SourceTikets.Next;
      if SourceTikets.Eof then
        TimerRead.Enabled := False;
    except
      TimerRead.Enabled := False;
    end;
  end;

var
  xPrice: Double;
  i, iCount: Integer;
begin
  iCount := 100;
  for i := 0 to iCount - 1 do
  begin
    if TimerRead.Enabled then
      xPrice := _Read
    else
      Break;
  end;

  ShowTradeBot;
  if xPrice > 0 then
    Text1.Text := TradeBot.GetProfit(xPrice).ToString;
end;


end.
