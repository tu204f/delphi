unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.TabControl,
  Lb.Mode,
  Lb.Position.Trade,
  UnitBotFrame,
  UnitIndicatorFrame, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TMainForm = class(TForm)
    LayoutMenu: TLayout;
    LayoutMain: TLayout;
    Layout: TLayout;
    ButtonStart: TButton;
    TabControl: TTabControl;
    TabItemBot: TTabItem;
    TabItemHistory: TTabItem;
    Timer: TTimer;
    LayoutLog: TLayout;
    Memo1: TMemo;
    procedure TimerTimer(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
  private
    OldStartTime: String;
    procedure SetStart;
    procedure SetStop;
  protected
    CurrentPostion: TTradePostion;
    procedure EventMode(Sender: TObject; AStartTime: String; APrice, AStopLoss: Double; AMode: TMode);
    procedure CurrentPostionOnClose(Sender: TObject);
  public
    Quantity: Double;
    BotFrame: TBotFrame;
    IndicatorFrame: TIndicatorFrame;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TForm5 }


constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  IndicatorFrame := TIndicatorFrame.Create(Self);
  IndicatorFrame.Parent := TabItemHistory;
  IndicatorFrame.Align := TAlignLayout.Client;
  IndicatorFrame.OnMode := EventMode;

  BotFrame := TBotFrame.Create(Self);
  BotFrame.Parent := TabItemBot;
  BotFrame.Align := TAlignLayout.Client;

end;


destructor TMainForm.Destroy;
begin
  FreeAndNil(BotFrame);
  FreeAndNil(IndicatorFrame);
  inherited;
end;

procedure TMainForm.SetStart;
begin
  Quantity := 0.001;
  OldStartTime := '';
  CurrentPostion := nil;
  ButtonStart.Text := 'Стоп';
  if IndicatorFrame.UpDataTimer then
    Timer.Enabled := True;
end;

procedure TMainForm.SetStop;
begin
  ButtonStart.Text := 'Старт';
  Timer.Enabled := False;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  if Timer.Enabled then
    SetStop
  else
    SetStart;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  if not IndicatorFrame.UpDataTimer then
    SetStop;

  if not BotFrame.UpDataTimer then
    SetStop;

  if Assigned(IndicatorFrame.CurrentCandel) then
  begin
    BotFrame.SetUpPrice(
      StrToInt64(IndicatorFrame.CurrentCandel.startTime),
      IndicatorFrame.CurrentCandel.Close
    );
  end;
end;

procedure TMainForm.EventMode(Sender: TObject; AStartTime: String; APrice, AStopLoss: Double; AMode: TMode);
var
  xS: String;
begin
  // Событие открытие позиции
  if not SameText(AStartTime,OldStartTime) then
  begin
    OldStartTime := AStartTime;

    xS := AStartTime;
    xS := xS + ' Price: ' + FloatToStr(APrice);
    xS := xS + ' SL: ' + FloatToStr(AStopLoss);
    case AMode of
      tmBuy : xS := xS + ' buy';
      tmSell: xS := xS + ' sell';
    end;

    Memo1.Lines.Add(xS);

    if not Assigned(CurrentPostion) then
    begin
      CurrentPostion := BotFrame.TradePostions.GetOpen(
        StrToInt64(AStartTime),
        APrice,
        Quantity,
        AMode,
        AStopLoss,
        AStopLoss
      );
      CurrentPostion.TypeStopLoss := TTypeStopLoss.slTrailingStop;
      CurrentPostion.Commission := 0.01;
      CurrentPostion.OnClose := CurrentPostionOnClose;
    end;

  end;
end;

procedure TMainForm.CurrentPostionOnClose(Sender: TObject);
begin
  Quantity := 0.001;
  CurrentPostion := nil;
end;

end.
