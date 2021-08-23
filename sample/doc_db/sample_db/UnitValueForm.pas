unit UnitValueForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TValueForm = class(TForm)
    ButtonApply: TButton;
    ButtonCancel: TButton;
    ValueEdit: TEdit;
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetValueForm(const ACaption: String; const AValue: String = ''): String;

implementation

{$R *.dfm}

function GetValueForm(const ACaption: String; const AValue: String = ''): String;
var
  xValueForm: TValueForm;
begin
  xValueForm := TValueForm.Create(nil);
  try
    xValueForm.Caption := ACaption;
    xValueForm.ValueEdit.Text := AValue;
    if xValueForm.ShowModal = mrOk then
      Result := xValueForm.ValueEdit.Text;
  finally
    FreeAndNil(xValueForm);
  end;
end;

procedure TValueForm.ButtonApplyClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TValueForm.ButtonCancelClick(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
end;

end.
