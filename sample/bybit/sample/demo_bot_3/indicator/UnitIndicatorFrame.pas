unit UnitIndicatorFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Layouts,
  FMX.Objects,
  FMX.Edit,
  Lb.Bybit.Kline,
  Lb.HistoryIndicator;

type
  TIndicatorFrame = class(TFrame)
    Layout: TLayout;
    TextTitle: TText;
    GridLayout: TGridPanelLayout;
    TextTime: TText;
    EditTime: TEdit;
    EditRSI: TEdit;
    EditAvgRSI: TEdit;
    Rectangle: TRectangle;
    TimerIsConnect: TTimer;
    TextClose: TText;
    EditClose: TEdit;
    TextVolRSI: TText;
    TextRSI: TText;
    procedure TimerIsConnectTimer(Sender: TObject);
  private
    OldCurrentTime, CurrentTime: String;
    HistoryIndicator: THistoryIndicator;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SetUpDate(const AHistoryIndicator: THistoryIndicator): Boolean;
  end;

implementation

{$R *.fmx}


{ TIndicatorFrame }

constructor TIndicatorFrame.Create(AOwner: TComponent);
begin
  inherited;
  HistoryIndicator := nil;

  CurrentTime := '';
  OldCurrentTime := '';
end;

destructor TIndicatorFrame.Destroy;
begin

  inherited;
end;

function TIndicatorFrame.SetUpDate(const AHistoryIndicator: THistoryIndicator): Boolean;
var
  xValueRSI, xAvgValueRSI: Double;
  iCount: Integer;
  xCandel: TCandelObject;
begin
  Result := True;
  try
    HistoryIndicator := AHistoryIndicator;
    iCount := HistoryIndicator.Candels.Count;
    if iCount > 0 then
    begin
      xCandel := HistoryIndicator.CurrentCandel;

      CurrentTime := HistoryIndicator.BybitKline.Response.CurrentTime;
      TextTitle.Text := 'Текущие значение: ' + CurrentTime;

      EditTime.Text   := DateTimeToStr(xCandel.DateTime);
      EditClose.Text  := FloatToStr(xCandel.Close);

      xValueRSI := HistoryIndicator.RSI.Values[0];
      xAvgValueRSI := HistoryIndicator.RSI.AvgValues[0];

      EditRSI.Text    := FloatToStr(xValueRSI);
      EditAvgRSI.Text := FloatToStr(xAvgValueRSI);
    end;
  except
    Result := False;
  end;
end;

procedure TIndicatorFrame.TimerIsConnectTimer(Sender: TObject);
begin
  // Проверяем наличие соединение
  if SameText(CurrentTime,OldCurrentTime) then
    TextTitle.TextSettings.FontColor := TAlphaColorRec.Red
  else
    TextTitle.TextSettings.FontColor := TAlphaColorRec.Green;
  OldCurrentTime := CurrentTime;
end;

end.
