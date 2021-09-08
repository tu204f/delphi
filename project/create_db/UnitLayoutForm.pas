unit UnitLayoutForm;

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
  FMX.Layouts;

type
  TLayoutForm = class(TForm)
    Layout: TLayout;
  private
    { Private declarations }
  public
    { Public declarations }
    class function GetShowForm(const ACaption: String; const AParent: TControl): TModalResult; static;
  end;

implementation

{$R *.fmx}

{ TLayoutForm }

class function TLayoutForm.GetShowForm(const ACaption: String; const AParent: TControl): TModalResult;
var
  xLayoutForm: TLayoutForm;
begin
  Result := mrNone;
  if Assigned(AParent) then
  begin
    xLayoutForm := TLayoutForm.Create(nil);
    try
      AParent.Parent := xLayoutForm.Layout;
      AParent.Align := TAlignLayout.Client;
      xLayoutForm.Caption := ACaption;
      Result := xLayoutForm.ShowModal;
      AParent.Parent := nil;
    finally
      FreeAndNil(xLayoutForm);
    end;
  end;
end;

end.
