unit Lb.DomainTableFrame;

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
  TDomainTableFrame = class(TFrame)
    Layout: TLayout;
    LayoutMenu: TLayout;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonDelete: TButton;
    StringGrid: TStringGrid;
  private
    FDomains: TCrDomains;
    procedure SetDomains(const Value: TCrDomains);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Domains: TCrDomains read FDomains write SetDomains;
  end;

implementation

{$R *.fmx}

{ TDomainTableFrame }

constructor TDomainTableFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TDomainTableFrame.Destroy;
begin

  inherited Destroy;
end;

procedure TDomainTableFrame.SetDomains(const Value: TCrDomains);
begin
  FDomains := Value;
end;

end.
