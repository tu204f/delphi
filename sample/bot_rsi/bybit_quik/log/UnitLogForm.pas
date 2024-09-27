unit UnitLogForm;

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
  FMX.Dialogs, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo;

type
  TLogForm = class(TForm)
    Memo: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FShowLog: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ShowLog: Boolean read FShowLog;
  end;

procedure SetClearLoggerForm;
procedure SetAddLoggerForm(S: String);

implementation

{$R *.fmx}

var
  localLogForm: TLogForm = nil;

procedure SetClearLoggerForm;
begin
  if not Assigned(localLogForm) then
  begin
    localLogForm := TLogForm.Create(Application);
  end;

  if not localLogForm.ShowLog then
    localLogForm.Show;
  localLogForm.Memo.Lines.Clear;
end;

procedure SetAddLoggerForm(S: String);
begin
  if not Assigned(localLogForm) then
  begin
    localLogForm := TLogForm.Create(Application);
  end;
  if not localLogForm.ShowLog then
    localLogForm.Show;
  localLogForm.Memo.Lines.Add(S);
end;

{ TLogForm }

constructor TLogForm.Create(AOwner: TComponent);
begin
  inherited;
  FShowLog := False;
end;

destructor TLogForm.Destroy;
begin

  inherited;
end;

procedure TLogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FShowLog := False;
end;

procedure TLogForm.FormShow(Sender: TObject);
begin
  FShowLog := True;
end;

initialization


finalization
  FreeAndNil(localLogForm);

end.
