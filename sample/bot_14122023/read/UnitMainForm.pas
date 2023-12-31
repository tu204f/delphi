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
  Lb.ReadPrice,
  FMX.Objects,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo;

type
  TMainForm = class(TForm)
    ButtonRead: TButton;
    TextStatus: TText;
    Memo1: TMemo;
    ButtonTikits: TButton;
    procedure ButtonReadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonTikitsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SourceCandels: TSourceCandels;
    SourceTiket: TSourceTiket;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SourceCandels := TSourceCandels.Create;
  SourceTiket := TSourceTiket.Create;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(SourceTiket);
  FreeAndNil(SourceCandels);
end;

procedure TMainForm.ButtonReadClick(Sender: TObject);

  procedure _LoadMemo;
  var
    xS: String;
    xCandel: TCandel;
    xParam: TParam;
    i, iCount: Integer;
  begin
    Memo1.Lines.Clear;
    iCount := SourceCandels.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel := SourceCandels.Candels[i];
        xParam := GetParamToCandel(xCandel);

        xS := IntToStr(i + 1) + ';' + ' ' +

          xCandel.Open.ToString + ' ' +
          xCandel.High.ToString + ' ' +
          xCandel.Low.ToString + ' ' +
          xCandel.Close.ToString + ' ' +
          xCandel.Vol.ToString + ' :: ' +

          xParam.GetSum.ToString + ' ' +
          xParam.T.ToString + ' ' +
          xParam.H.ToString + ' ' +
          xParam.B.ToString + ' ' +
          xParam.L.ToString;

        Memo1.Lines.Add(xS);
      end;
  end;

var
  xFileName: String;
begin
  xFileName := 'd:\work\git\delphi\sample\bot_14122023\data\SRZ3202211_13122023_14122023(1).txt';
  SourceCandels.LoadFromFile(xFileName);
  SourceCandels.Delete(0);
  TextStatus.Text := 'Количество строк:' + SourceCandels.Count.ToString;
  _LoadMemo;
end;



procedure TMainForm.ButtonTikitsClick(Sender: TObject);

  procedure _LoadMemo;
  var
    xS: String;
    xTiket: TTiket;
    i, iCount: Integer;
  begin
    Memo1.Lines.Clear;
    iCount := SourceTiket.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xTiket := SourceTiket.Tikets[i];


        xS := IntToStr(i + 1) + ';' + ' ' +
          xTiket.Time.ToString + ' ' +
          xTiket.Last.ToString + ' ' +
          xTiket.Vol.ToString;


        Memo1.Lines.Add(xS);
      end;
  end;

var
  xFileName: String;
begin
  xFileName := 'd:\work\git\delphi\sample\bot_14122023\data\SRZ3202211_13122023_13122023.txt';
  SourceTiket.LoadFromFile(xFileName);
  SourceTiket.Delete(0);
  TextStatus.Text := 'Количество строк:' + SourceTiket.Count.ToString;
  _LoadMemo;
end;

end.
