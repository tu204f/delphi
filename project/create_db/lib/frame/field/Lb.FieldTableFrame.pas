unit Lb.FieldTableFrame;

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
  Lb.Create.DB,
  System.Rtti,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Controls.Presentation,
  FMX.Layouts;

type
  TFieldTableFrame = class(TFrame)
    Layout: TLayout;
    LayoutMenu: TLayout;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonDelete: TButton;
    StringGrid: TStringGrid;
  private
    FFields: TCrFields;
    procedure SetFields(const Value: TCrFields);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Fields: TCrFields read FFields write SetFields;
  end;

implementation

{$R *.fmx}

{ TFieldTableFrame }

constructor TFieldTableFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFields := nil;
end;

destructor TFieldTableFrame.Destroy;
begin

  inherited;
end;

procedure TFieldTableFrame.SetFields(const Value: TCrFields);
begin
  FFields := Value;
end;

end.
