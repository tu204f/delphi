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
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  
  Lb.SysUtils, 
  Lb.Breakdown,
  Lb.Bot,
  
  FMX.Memo.Types, 
  FMX.ScrollBox, 
  FMX.Memo, 
  FMX.Objects, 
  FMX.Layouts,
  UnitPositionFrame;

type
  TMainForm = class(TForm)
    ButtonActive: TButton;
    Timer: TTimer;
    Text1: TText;
    LayoutPosition: TLayout;
    Text2: TText;
    procedure ButtonActiveClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FCurrentLine: TSourceLine;
    FSourceStreame: TSourceStreame;
    FWorkBot: TWorkBot;
    FPositionFrame: TPositionFrame;
    procedure EventSourceStreame(Sender: TObject; ASourceLine: TSourceLine);
  protected
    procedure DoStart;
    procedure DoStop;
    procedure StartSourceStreame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSourceStreame := TSourceStreame.Create;
  FSourceStreame.OnEventSourceStreame := EventSourceStreame;

  FWorkBot := TWorkBot.Create;
  FWorkBot.Rate := 0.5;

  FPositionFrame := TPositionFrame.Create(nil);
  FPositionFrame.Parent := LayoutPosition;
  FPositionFrame.Align := TAlignLayout.Client;
  FPositionFrame.WorkBot := FWorkBot;
  
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FPositionFrame);
  FreeAndNil(FWorkBot);
  FreeAndNil(FSourceStreame);
  inherited;
end;

procedure TMainForm.DoStart;
begin
  ButtonActive.Text := 'Стоп';
  Timer.Enabled := True;
  FSourceStreame.Open('history.txt');
  StartSourceStreame;
end;

procedure TMainForm.StartSourceStreame;
begin
  FSourceStreame.First;
  while FCurrentLine.Deviation = 0 do
  begin
    FSourceStreame.Next;
  end;
end;

procedure TMainForm.DoStop;
begin
  ButtonActive.Text := 'Старт';
  Timer.Enabled := False; 
end;

procedure TMainForm.EventSourceStreame(Sender: TObject; ASourceLine: TSourceLine);
begin
  if FCurrentLine.Candel.TimeValue <> ASourceLine.Candel.TimeValue then
    FWorkBot.ClosePosition;

  FCurrentLine := ASourceLine;

  if FCurrentLine.Deviation = 0 then
    Exit;

  FWorkBot.SetTradingPlatform(FCurrentLine);
  FPositionFrame.SetTradingPlatform(FCurrentLine);

  if FSourceStreame.EOF then
  begin
    DoStop;
    FWorkBot.ClosePosition;
  end;

  Text1.Text :=
    FCurrentLine.Candel.TimeValue.ToString + '/' +
    FCurrentLine.Deviation.ToString + '/' +
    FCurrentLine.Ask.ToString + '/' +
    FCurrentLine.Bid.ToString;

  Text2.Text :=
    FPositionFrame.Profit.ToString + ' // ' +
    FPositionFrame.ProfitFeeRatesTaker.ToString + ' // ' +
    FPositionFrame.ProfitFeeRatesMaker.ToString;

end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  try
    for var i := 0 to 99 do
    begin
      FSourceStreame.Next;
    end;

  except
    on E: Exception do
    begin
      DoStop;
      raise Exception.Create('Error Message: Ошибки в потоке' + sLineBreak + E.Message);
    end;
  end;
end;

procedure TMainForm.ButtonActiveClick(Sender: TObject);
begin
  if Timer.Enabled then
    DoStop
  else
    DoStart;
end;

end.
