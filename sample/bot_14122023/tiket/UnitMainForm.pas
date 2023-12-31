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
  Lb.Bot.Tiket;

type
  TMainForm = class(TForm)
    Memo1: TMemo;
    Chart1: TChart;
    Series1: TLineSeries;
    TextStatus: TText;
    ButtonRead: TButton;
    TimerRead: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerReadTimer(Sender: TObject);
    procedure ButtonReadClick(Sender: TObject);
  private
    { Private declarations }
    IndexTiket: Integer;
  public
    BeginTime: Integer;
    SourceTikets: TSourceTikets;
    Bot: TTakeProfitTiketBot;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Bot := TTakeProfitTiketBot.Create;
  Bot.StopLoss := -10;
  Bot.CountStop := 3;
  Bot.TakeProfit := 30;

  SourceTikets := TSourceTikets.Create;
  BeginTime := 32400;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(SourceTikets);
  FreeAndNil(Bot);
end;

procedure TMainForm.ButtonReadClick(Sender: TObject);
var
  xFileName: String;
begin
  Memo1.Lines.Clear;
  Bot.Clear;
  Series1.Clear;

  xFileName := 'd:\work\git\delphi\sample\bot_14122023\data\SRZ3202211_13122023_13122023.txt';
  SourceTikets.LoadFromFile(xFileName);
  SourceTikets.Delete(0);
  TextStatus.Text := ' оличество строк:' + SourceTikets.Count.ToString;

  BeginTime := 32400;
  IndexTiket := 0;
  TimerRead.Enabled := not TimerRead.Enabled;
end;


procedure TMainForm.TimerReadTimer(Sender: TObject);
var
  xS: String;
  xTiket: TTiket;
  xCandel: TCandel;
  xBeginTime, xEndTime: Integer;
begin
  try
    xS := '';
  
    xBeginTime := BeginTime;
    xEndTime   := xBeginTime + 300;


    xTiket := SourceTikets.Tikets[IndexTiket];
    xCandel.Open  := xTiket.Last;
    xCandel.High  := xTiket.Last;
    xCandel.Low   := xTiket.Last;
    xCandel.Close := xTiket.Last;
    xCandel.Vol   := xTiket.Vol;

    if not Bot.IsPosition then
      Bot.SetOpenPosition(xTiket.Last);

    for var i := IndexTiket to SourceTikets.Count - 1 do
    begin
      IndexTiket := i;
      xTiket := SourceTikets.Tikets[i];

      Bot.SetUpPosition(xTiket.Last);

      if (xBeginTime >= xTiket.Time) and (xTiket.Time < xEndTime) then
      begin
        if xCandel.High < xTiket.Last then
          xCandel.High := xTiket.Last;
        if xCandel.Low  > xTiket.Last then
          xCandel.Low  := xTiket.Last;
        xCandel.Vol := xCandel.Vol;


        
        {маханиз, нужно изменить}
        xS := IntToStr(i + 1) + ';' + ' ' +

            xCandel.Open.ToString + ' ' +
            xCandel.High.ToString + ' ' +
            xCandel.Low.ToString + ' ' +
            xCandel.Close.ToString + ' ' +
            xCandel.Vol.ToString;

      end
      else
      begin
        // ¬ конце св€чи принудительно закрываем позицию
        if Bot.IsPosition then
        begin
          Bot.SetClosePosition(xTiket.Last);
          Series1.AddY(Bot.HealthPoints);
        end;

        Memo1.Lines.Add(xS);
        IndexTiket := IndexTiket - 1;
        Break;
      end;
    end;


    if not Bot.IsPosition then
      Bot.SetMode(xCandel);

    BeginTime  := xEndTime;

    if SourceTikets.Count < xBeginTime then
      TimerRead.Enabled := False;

  except
    TimerRead.Enabled := False;
  end;
end;


end.
