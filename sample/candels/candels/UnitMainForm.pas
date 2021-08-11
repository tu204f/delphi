unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo.Types, FMX.StdCtrls, FMX.Memo;

type
  TMainForm = class(TForm)
    TabControl: TTabControl;
    TabItemLoad: TTabItem;
    TabItem2: TTabItem;
    Memo1: TMemo;
    ButtonLoad: TButton;
    Timer: TTimer;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.ChartsFrame,
  Lb.Candel.Source, Lb.Candel.Vector, Lb.Candel.DB;

var
  localCandels: TSourceCandel;
  localIndex: Integer = 0;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Self.Caption := 'Генерирование формы свечей';
  TChartsFrame.GetCreateFrame(TabItem2);
end;


procedure TMainForm.TimerTimer(Sender: TObject);
begin
  Inc(localIndex);
  TChartsFrame.SetShowCharts(localIndex,100);

  if localIndex > 700 then
    Timer.Enabled := False;

end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  localIndex := 0;
  var xStr := TStringList.Create;
  try
    // ----------------------------------
    xStr.LoadFromFile('export.csv');
    for var S in xStr do
    begin
      Memo1.Lines.Add(S);
    end;
    // ----------------------------------
    localCandels := TChartsFrame.GetSource;
    localCandels.SetParserCandels(xStr);
    TChartsFrame.SetShowCharts(localIndex,100);
    // ----------------------------------
  finally
    FreeAndNil(xStr);
  end;

  Timer.Enabled := True;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  SetCreateDataBase;
end;

procedure TMainForm.ButtonLoadClick(Sender: TObject);
begin
  var xStr := TStringList.Create;
  try
    // ----------------------------------
    xStr.LoadFromFile('export.csv');
    for var S in xStr do
    begin
      Memo1.Lines.Add(S);
    end;
    // ----------------------------------
    var xCandels := TChartsFrame.GetSource;
    xCandels.SetParserCandels(xStr);
    TChartsFrame.SetShowCharts(0,100);


    // ----------------------------------

    for var i := 10 to xCandels.Candels.Count - 1 do
    begin
      var xVector := TEnvelopeVector.Create;
      xVector.SetCandels(i,10,3,xCandels.Candels);
      xVector.SetWrite;
      FreeAndNil(xVector);
    end;


    // ----------------------------------
  finally
    FreeAndNil(xStr);
  end;
end;


end.
