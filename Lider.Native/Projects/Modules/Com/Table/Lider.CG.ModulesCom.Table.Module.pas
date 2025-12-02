unit Lider.CG.ModulesCom.Table.Module;

interface

uses
  System.Classes,
  SysUtils,
  System.UITypes,
  System.Types,
  TypInfo;
//  Lider.CG.ModulesCom.Table.TableInt;

type
  EnumerableExtensions = class
    private
      constructor Create;
    public
      function FindIndex<T>(list: IEnumerable<T>; finder: TPredicate<T>): Integer;
  end;

type
  Module1 = class(TDataModule)
    public
      function ToHtmlHexadecimal(color: TColor): String;
      function CheckColor(ColorString: String): Boolean;
      //Enum Araçlarý
      function getEnumValue(enumType: TTypeInfo; value: String): TObject;
  end;

implementation
{ EnumerableExtensions }

constructor EnumerableExtensions.Create;
begin

end;

function EnumerableExtensions.FindIndex<T>(list: IEnumerable<T>;
  finder: TPredicate<T>): Integer;
begin
//  Return list.TakeWhile(Function(i) Not finder(i)).Count
end;

{ Module1 }

function Module1.CheckColor(ColorString: String): Boolean;
begin
  (*
  Static htmlColorRegex As New Regex("^#((?'R'[0-9a-f]{2})(?'G'[0-9a-f]{2})(?'B'[0-9a-f]{2}))" + "|((?'R'[0-9a-f])(?'G'[0-9a-f])(?'B'[0-9a-f]))$", RegexOptions.Compiled Or RegexOptions.IgnoreCase)
  Try
      If ColorString Is Nothing Then
          Throw New ArgumentNullException("colorString")
      End If

      Dim match As Match = htmlColorRegex.Match(ColorString)
      If Not match.Success Then
          Dim msg = "The string \ {0} \  doesn't represent"
          msg += "a valid HTML hexadecimal color"
          msg = String.Format(msg, ColorString)

          Throw New ArgumentException(msg, "colorString")
      Else
          Return True
      End If
  Catch ex As Exception
      'MsgBox(ex.Message)
      Return False
  End Try
  *)
end;

function Module1.getEnumValue(enumType: TTypeInfo; value: String): TObject;
begin
  {
  Dim names As String = System.Enum.GetNames(enumType)
  For Each name As String In names
      Debug.Write(getEnumString(DirectCast(System.Enum.Parse(enumType, name), System.Enum)).ToString)
      Debug.Write(getEnumString(DirectCast(System.Enum.Parse(enumType, name), System.Enum)).Equals(value).ToString)
      If (getEnumString(DirectCast(System.Enum.Parse(enumType, name), System.Enum))).Equals(value) Then
          ' MsgBox(System.Enum.Parse(enumType, name).ToString)
          Return System.Enum.Parse(enumType, name)
      End If
  Next name
  Throw New ArgumentException("The string is not an EnumMember or name of an enum member of the specified enum.")
  }
end;

function Module1.ToHtmlHexadecimal(color: TColor): String;
begin
//  Return String.Format("#{0:X2}{1:X2}{2:X2}", color.R, color.G, color.B)
end;

end.

