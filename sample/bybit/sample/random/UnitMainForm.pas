unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Rtti, FMX.Grid.Style,
  FMX.ScrollBox, FMX.Grid,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  Lb.SysUtils.Candel,
  Lb.Bot.Tiket,
  FMX.Memo.Types,
  FMX.Memo, FMX.Objects;

type
  TMainForm = class(TForm)
    ButtonStart: TButton;
    StringGrid1: TStringGrid;
    sgTrade: TStringGrid;
    TextOpenPrice: TText;
    TextClosePrice: TText;
    TextBuySell: TText;
    TextChange: TText;
    TextCurrentPrice: TText;
    Text1: TText;
    Text2: TText;
    Text3: TText;
    Text4: TText;
    Text5: TText;
    TextHP: TText;
    Text6: TText;
    procedure ButtonStartClick(Sender: TObject);
  private
    Bot: TTakeProfitTiketBot;
    procedure SetTrades;
    procedure SetInfoTrade(const ALast: Double);
  protected
    procedure SetNewCandel(const ALast: Double);
    procedure SetUpCandel(const ALast: Double);
  protected
    StartTime: String;
    BybitKline: TBybitKline;
    procedure BybitKlineOnEventEndLoading(Sender: TObject);
    procedure BotOnOpenPosition(Sender: TObject);
    procedure BotOnClosePosition(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{$IFDEF DEBUG}
uses
  Lb.Logger;
{$ENDIF}

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);

  procedure _AddColumn(AGrid: TStringGrid; AHeader: String; AWidth: Double);
  var
    xColumn: TColumn;
  begin
    xColumn := TColumn.Create(AGrid);
    xColumn.Header := AHeader;
    xColumn.Parent := AGrid;
    xColumn.Width  := AWidth;
  end;

begin
  inherited;
  BybitKline := TBybitKline.Create;
  BybitKline.OnEventEndLoading := BybitKlineOnEventEndLoading;

  _AddColumn(StringGrid1,'startTime',120);
  _AddColumn(StringGrid1,'openPrice',100);
  _AddColumn(StringGrid1,'highPrice',90);
  _AddColumn(StringGrid1,'lowPrice',90);
  _AddColumn(StringGrid1,'closePrice',90);
  _AddColumn(StringGrid1,'volume',90);
  _AddColumn(StringGrid1,'turnover',90);

  _AddColumn(sgTrade,'Time',80);
  _AddColumn(sgTrade,'Price',80);
  _AddColumn(sgTrade,'Quantity',80);
  _AddColumn(sgTrade,'BuySell',80);
  _AddColumn(sgTrade,'Status',80);

  Bot := TTakeProfitTiketBot.Create;
  Bot.OnOpenPosition  := BotOnOpenPosition;
  Bot.OnClosePosition := BotOnClosePosition;
  Bot.TakeProfit := 15;
  Bot.StopLoss := -5;
  Bot.CountStop := 3;
  Bot.Quantity := 1;

end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(Bot);
  FreeAndNil(BybitKline);
  inherited;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  StartTime := '';
  BybitKline.Category := TTypeCategory.tcLinear;
  BybitKline.Symbol   := 'BTCUSDT';
  BybitKline.Interval := TTypeInterval.ti_1;
  BybitKline.Limit    := 2;
  BybitKline.Selected(100);
end;

procedure TMainForm.BybitKlineOnEventEndLoading(Sender: TObject);
var
  xCandel: TCandel;
  xStartTime: String;
  i, iCount: Integer;
  xCandelObject: TCandelObject;
  xCandelObjects: TCandelObjectList;
begin
  xCandelObjects := TCandelObjectList.Create;
  try
    SetLinearObjects(BybitKline.ListJson,xCandelObjects);
    iCount := xCandelObjects.Count;
    if iCount > 0 then
    begin
      xCandelObject := xCandelObjects[0];

      xCandel.Date := xCandelObject.DateTime;
      xCandel.Time := xCandelObject.DateTime;

      FormatSettings.DecimalSeparator := '.';
      xCandel.Open  := StrToFloatDef(xCandelObject.openPrice,0);
      xCandel.High  := StrToFloatDef(xCandelObject.highPrice,0);
      xCandel.Low   := StrToFloatDef(xCandelObject.lowPrice,0);
      xCandel.Close := StrToFloatDef(xCandelObject.closePrice,0);
      xCandel.Vol   := StrToFloatDef(xCandelObject.volume,0);
      FormatSettings.DecimalSeparator := ',';

      xStartTime := xCandelObject.startTime;
      if SameText(StartTime,xStartTime) then
      begin
        // Обновление
        SetUpCandel(xCandel.Close);
      end
      else
      begin
        // Новая свеча
        StartTime := xStartTime;
        SetNewCandel(xCandel.Close);
      end;

      StringGrid1.RowCount := iCount;
      for i := 0 to iCount - 1 do
      begin
        xCandelObject := xCandelObjects[i];
        StringGrid1.Cells[0,i] := DateTimeToStr(xCandelObject.DateTime);
        StringGrid1.Cells[1,i] := xCandelObject.openPrice;
        StringGrid1.Cells[2,i] := xCandelObject.highPrice;
        StringGrid1.Cells[3,i] := xCandelObject.lowPrice;
        StringGrid1.Cells[4,i] := xCandelObject.closePrice;
        StringGrid1.Cells[5,i] := xCandelObject.volume;
        StringGrid1.Cells[6,i] := xCandelObject.turnover;
      end;
    end;
  finally
    FreeAndNil(xCandelObjects);
  end;
end;

procedure TMainForm.SetTrades;
var
  xTrade: TPosition.TTrade;
  i, iCount, xInd: Integer;
begin
  iCount := Bot.Position.Trades.Count;
  if iCount > 0 then
  begin
    sgTrade.RowCount := iCount;
    for i := 0 to iCount - 1 do
    begin
      xInd := (iCount - 1) - i;
      xTrade := Bot.Position.Trades[xInd];

      sgTrade.Cells[0,i] := FormatDateTime('hh:mm:ss.zzz',xTrade.Time);
      sgTrade.Cells[1,i] := xTrade.Price.ToString;
      sgTrade.Cells[2,i] := xTrade.Quantity.ToString;
      sgTrade.Cells[3,i] := xTrade.BuySell;
      case xTrade.Status of
        TPosition.TRD_OPEN : sgTrade.Cells[4,i] := 'open';
        TPosition.TRD_CLOSE: sgTrade.Cells[4,i] := 'close';
      end;

    end;
  end;
end;

function _Round(const AValue: Double): Double;
begin
  Result := Round(AValue * 100)/100;
end;

procedure TMainForm.SetInfoTrade(const ALast: Double);
var
  xValue: Double;
begin
  xValue := 0;
  TextOpenPrice.Text := Bot.OpenPrice.ToString;
  TextClosePrice.Text := '';

  case Bot.Mode of
    tmBuy: begin
      xValue := ALast - Bot.OpenPrice;
      TextBuySell.Text := 'Покупка';
    end;
    tmSell: begin
      xValue := Bot.OpenPrice - ALast;
      TextBuySell.Text := 'Продажа';
    end;
  end;
  TextChange.Text := _Round(xValue).ToString;

  if xValue > 0 then
    TextBuySell.Color := TAlphaColorRec.Green
  else
    TextBuySell.Color := TAlphaColorRec.Red;

  TextCurrentPrice.Text := ALast.ToString;
end;

procedure TMainForm.SetNewCandel(const ALast: Double);
begin
{$IFDEF DEBUG}
  TLogger.LogText('*',80);
  TLogger.LogTree(0,'TMainForm.SetNewCandel');
  TLogger.LogTreeText(3,'>> Last: ' + ALast.ToString);
{$ENDIF}
  if (ALast = 0) or (ALast = Bot.OpenPrice) then
    Exit;

  SetInfoTrade(ALast);

//  if Bot.IsPosition then
//  begin
//    {$IFDEF DEBUG}
//    TLogger.LogTreeText(3,'>> Принудительное закрытие позиции');
//    {$ENDIF}
//    Bot.SetClosePosition(ALast);
//  end;

  if not Bot.IsPosition then
  begin
    {$IFDEF DEBUG}
    TLogger.LogTreeText(3,'>> Открываем позицию');
    {$ENDIF}
    Bot.SetMode;
    Bot.SetOpenPosition(ALast);
    SetTrades;
  end;

  {$IFDEF DEBUG}
  if Bot.IsPosition then
    TLogger.LogTreeText(3,'-->> Позиция открыта')
  else
    TLogger.LogTreeText(3,'-->> Позиция закрыта');
  {$ENDIF}
end;

procedure TMainForm.SetUpCandel(const ALast: Double);
begin
{$IFDEF DEBUG}
  TLogger.LogTree(0,'TMainForm.SetUpCandel');
  TLogger.LogTreeText(3,'>> Last: ' + ALast.ToString);
{$ENDIF}
  if Bot.IsPosition then
    Bot.SetUpPosition(ALast);
  SetInfoTrade(ALast);
end;

procedure TMainForm.BotOnOpenPosition(Sender: TObject);
begin

end;

procedure TMainForm.BotOnClosePosition(Sender: TObject);
begin
(*
Есть простая формула расчета риска и прибыли.
(ТВХ/TP)-1)*100 * плечо * %от депозита это прибыль.
(ТВХ/SL)-1)*100 * плечо * %от депозита это риск.

*)

  TextHP.Text := _Round(Bot.HealthPoints).ToString;
  SetTrades;
end;


end.
