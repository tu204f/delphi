unit Lb.MethodTableFrame;

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
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Controls.Presentation,
  FMX.Layouts,
  Lb.Create.DB;

type
  TMethodTableFrame = class(TFrame)
    Layout: TLayout;
    LayoutMenu: TLayout;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonDelete: TButton;
    StringGrid: TStringGrid;
  private
    FMethods: TCrMethod;
    procedure SetMethods(const Value: TCrMethod);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Methods: TCrMethod read FMethods write SetMethods;
  end;

implementation

{$R *.fmx}

{ TMethodTableFrame }

constructor TMethodTableFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMethods := nil;
end;

destructor TMethodTableFrame.Destroy;
begin

  inherited;
end;

procedure TMethodTableFrame.SetMethods(const Value: TCrMethod);
begin
  FMethods := Value;
end;

end.
