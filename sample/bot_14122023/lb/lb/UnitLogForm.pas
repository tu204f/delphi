unit UnitLogForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TLogForm = class(TForm)
    LogMemo: TMemo;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    IsShow: Boolean;
  public
    { Public declarations }
  end;

var
  LogForm: TLogForm;

procedure SetLog(const S: String);

implementation

{$R *.fmx}

procedure SetLog(const S: String);
var
  xS: String;
begin
//  if not LogForm.IsShow then
//    LogForm.Show;

  if LogForm.IsShow then
  begin
    xS := FormatDateTime('hh:mm:ss.zzz',Time) + ' :: ' + S;
    LogForm.LogMemo.Lines.Add(xS);
    var iCount := LogForm.LogMemo.Lines.Count;
    if iCount > 100 then
      LogForm.LogMemo.Lines.Delete(0);
  end;
end;

procedure TLogForm.FormShow(Sender: TObject);
begin
  IsShow := True;
end;

procedure TLogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IsShow := False;
end;

end.
