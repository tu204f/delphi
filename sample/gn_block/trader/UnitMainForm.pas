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
  Lb.ReadPrice,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects,
  System.Rtti,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.TreeView,
  UnitTraderFrame;

type
  TMainForm = class(TForm)
    ButtonReadPrice: TButton;
    Timer: TTimer;
    ListBox: TListBox;
    Layout: TLayout;
    procedure ButtonReadPriceClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    CurrentDate: TDateTime;
    CandelIndex: Integer;
    CandelsSource: TCandelsSource;
  private
    TraderFrame: TTraderFrame;
  protected
    procedure SetPriceLog(const S: String);
    procedure SetStartCandel(ACandel: TCandel);
    procedure SetUpDateCandel(ACandel: TCandel);
    procedure SetAddCandel(ACandel: TCandel);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

function GetIndexToStr(const ACandelIndex: Integer): String;
var
  xS: String;
  xL: Integer;
begin
  xS := ACandelIndex.ToString;
  xL := xS.Length;
  case xL of
    1: Result := '000' + xS;
    2: Result := '00' + xS;
    3: Result := '0' + xS;
  else
    Result := xS;
  end;
end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TraderFrame := TTraderFrame.Create(nil);
  TraderFrame.Parent := Layout;
  TraderFrame.Align := TAlignLayout.Client;

  CandelsSource := TCandelsSource.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(CandelsSource);
  FreeAndNil(TraderFrame);
  inherited;
end;


procedure TMainForm.ButtonReadPriceClick(Sender: TObject);
var
  xFileName: String;
begin
  Timer.Enabled := not Timer.Enabled;
  if Timer.Enabled then
  begin
    CandelIndex := 1;
    ButtonReadPrice.Text := 'Стоп';
    xFileName := ExtractFilePath(ParamStr(0)) + 'data\';
    xFileName := xFileName + 'GAZP_240601_240804.csv';
    CandelsSource.LoadFromFile(xFileName);
    ListBox.Items.Clear;
  end
  else
    ButtonReadPrice.Text := 'Старт';
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  xCandel: TCandel;
begin
  try
    xCandel := CandelsSource.Candels[CandelIndex];
    if CandelIndex = 1 then
    begin
      SetStartCandel(xCandel);
      CurrentDate := xCandel.Date;
    end
    else
    begin
      if CurrentDate = xCandel.Date then
        SetUpDateCandel(xCandel)
      else
      begin
        SetStartCandel(xCandel);
        CurrentDate := xCandel.Date;
      end;
    end;
    Inc(CandelIndex);
    if CandelIndex >= CandelsSource.Count then
    begin
      Timer.Enabled := False;
      ButtonReadPrice.Text := 'Старт';
    end;
  except
    Timer.Enabled := False;
    ButtonReadPrice.Text := 'Старт.Error';
  end;
end;


procedure TMainForm.SetPriceLog(const S: String);
begin
  ListBox.Items.Add(S);
  if ListBox.Items.Count > 20 then
    ListBox.Items.Delete(0);
end;


procedure TMainForm.SetStartCandel(ACandel: TCandel);
var
  xS: String;
begin
  SetPriceLog('start');
  xS :=
    '[' + GetIndexToStr(CandelIndex) + '] ' +
    DateTimeToStr(ACandel.Date + ACandel.Time) + ' ' +
    ACandel.Close.ToString + ' ' +
    ACandel.Vol.ToString;

  SetPriceLog(xS);

  TraderFrame.SetStart;
  SetAddCandel(ACandel);
end;

procedure TMainForm.SetUpDateCandel(ACandel: TCandel);
var
  xS: String;
begin
  xS :=
    '[' + GetIndexToStr(CandelIndex) + '] ' +
    DateTimeToStr(ACandel.Date + ACandel.Time) + ' ' +
    ACandel.Close.ToString + ' ' +
    ACandel.Vol.ToString;

  SetPriceLog(xS);
  SetAddCandel(ACandel);
end;

procedure TMainForm.SetAddCandel(ACandel: TCandel);
begin
  TraderFrame.SetUpCandel(ACandel);
end;

end.
