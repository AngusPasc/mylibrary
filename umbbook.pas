unit UMBBook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db;

type
  TMBBook = Class
    private
      FBookID : Integer;
      FBookName : String;
      FBookISBN : String;
      FBookYear : String;
      FBookNote : String;
      FBookCompositions : array of Integer;
      FBookAuthors : array of Integer;
      FBookPublisherID : Integer;
      FBookEditorID : Integer;
      FBookGenreID : Integer;
      FNewBook : Boolean;
      //
      procedure GetBookCompositions(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      function GetBookCompositionID( Index : Integer ) : Integer;
      //procedure GetBookAuthors(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      function GetBookAuthorID( Index : Integer ) : Integer;
      //procedure GetBookPublisherID(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      //procedure GetBookEditor(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      //procedure GetBookGenres();
      procedure SetBookName( Name : String );
      procedure SetBookISBN( ISBN : String );
      procedure SetBookYear( Year : String );
      procedure SetBookNote( Note : String );
      //
      procedure ReadBookAuthorsFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      procedure ReadBookEditorFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      procedure ReadBookGenresFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      procedure ReadBookPublisherFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      procedure ReadBookDataFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
    public
      constructor Create(); overload;
      constructor Create(BookName : String; BookISBN : String; BookYear : String; BookNote : String; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction); overload;
      constructor Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction); overload;
      property BookID : Integer read FBookID;
      property BookName : String read FBookName write SetBookName;
      property BookISBN : String read FBookISBN write SetBookISBN;
      property BookYear : String read FBookYear write SetBookYear;
      property BookNote : String read FBookNote write SetBookNote;
      property BookComposition[Index : Integer] : Integer read GetBookCompositionID;
      property BookAuthor[Index : Integer] : Integer read GetBookAuthorID;

      procedure AddGenre(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      procedure AddPublisher(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      procedure AddEditor(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      procedure AddComposition(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);

    published
  end;


implementation
uses
  Dialogs;

constructor TMBBook.Create();
begin
     inherited;
     FBookName := '';
     FBookISBN := '';
     FBookYear := '';
     FBookNote := '';
     FNewBook := False;
end;

constructor TMBBook.Create(BookName : String; BookISBN : String; BookYear : String; BookNote : String; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT id, isbn, year, note FROM books where name=:bName';
     SQLQuery.Params.ParamByName('bName').AsString:=BookName;
     SQLQuery.Open;
     if SQLQuery.RecordCount > 0 then
     begin
          FBookName:=BookName;
          FBookISBN:=BookISBN;
          FBookYear:=BookYear;
          FBookNote:=BookNote;
          FBookID:=SQLQuery.FieldByName('id').AsInteger;
          FNewBook := False;
          ReadBookDataFromDB(SQLQuery, SQLTransaction);
     end
     else
     begin
          FBookName := BookName;
          FBookISBN := BookISBN;
          FBookYear := BookYear;
          FBookNote := BookNote;
          FNewBook := True;
     end;

end;

constructor TMBBook.Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT name, isbn, year, note FROM books where id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(ID);
     SQLQuery.Open;
     //ДК, нужна проверка что запрос вернул не пустой результат
     FBookName:=SQLQuery.FieldByName('name').AsString;
     FBookISBN:=SQLQuery.FieldByName('isbn').AsString;
     FBookYear:=SQLQuery.FieldByName('year').AsString;
     FBookNote:=SQLQuery.FieldByName('note').AsString;
     FBookID:=ID;
     SQLQuery.Close;
     ReadBookDataFromDB(SQLQuery, SQLTransaction);
     FNewBook := False;

end;


procedure TMBBook.ReadBookAuthorsFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
var
  I, J : Integer;
begin
     if Length(FBookCompositions) > 0 then
     begin
          I:=0;
          for J:=0 to Length(FBookCompositions) do
          begin
               SQLQuery.Close;
               SQLQuery.SQL.Text:='SELECT author_id FROM rel_composition_authors where composition_id=:bID';
               SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FBookCompositions[J]);
               SQLQuery.Open;
               SQLQuery.Last;
               if SQLQuery.RecordCount > 0 then
               begin
                    SetLength(FBookAuthors, SQLQuery.RecordCount+Length(FBookAuthors));
                    SQLQuery.First;
                    while not SQLQuery.EOF do
                    begin
                         FBookAuthors[I]:=SQLQuery.FieldByName('author_id').AsInteger;
                         I:=I+1;
                         SQLQuery.Next;
                    end;
                    SQLQuery.Close;
               end;
          end;
     end;
end;

procedure TMBBook.ReadBookEditorFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT editor_id FROM rel_book_editors where book_id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FBookID);
     SQLQuery.Open;
     if SQLQuery.RecordCount > 0 then
     begin
          SQLQuery.Last;
          FBookEditorID:=SQLQuery.FieldByName('editor_id').AsInteger;
     end;
end;

procedure TMBBook.ReadBookPublisherFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT publisher_id FROM rel_publisher_books where book_id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FBookID);
     SQLQuery.Open;
     if SQLQuery.RecordCount > 0 then
     begin
          SQLQuery.Last;
          FBookPublisherID:=SQLQuery.FieldByName('publisher_id').AsInteger;
     end;
end;

procedure TMBBook.ReadBookGenresFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT genre_id FROM rel_book_genres where book_id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FBookID);
     SQLQuery.Open;
     if SQLQuery.RecordCount > 0 then
     begin
          SQLQuery.Last;
          FBookGenreID:=SQLQuery.FieldByName('genre_id').AsInteger;
     end;
end;

procedure TMBBook.ReadBookDataFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     GetBookCompositions(SQLQuery, SQLTransaction);
     ReadBookAuthorsFromDB(SQLQuery, SQLTransaction);
     ReadBookEditorFromDB(SQLQuery, SQLTransaction);
     ReadBookPublisherFromDB(SQLQuery, SQLTransaction);
     ReadBookGenresFromDB(SQLQuery, SQLTransaction);
end;

procedure TMBBook.GetBookCompositions(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
var
  I : Integer;
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT composition_id FROM rel_book_compositions where book_id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FBookID);
     SQLQuery.Open;
     SQLQuery.Last;
     if SQLQuery.RecordCount > 0 then
     begin
          SetLength(FBookCompositions, SQLQuery.RecordCount);
          SQLQuery.First;
          I:=0;
          while not SQLQuery.EOF do
          begin
               FBookCompositions[I]:=SQLQuery.FieldByName('composition_id').AsInteger;
               I:=I+1;
               SQLQuery.Next;
          end;
          SQLQuery.Close;
     end;
end;

function TMBBook.GetBookCompositionID( Index : Integer ) : Integer;
begin
     Result := FBookCompositions[Index];
end;

function TMBBook.GetBookAuthorID( Index : Integer ) : Integer;
begin
     Result := FBookAuthors[Index];
end;

procedure TMBBook.SetBookName( Name : String );
begin
     FBookName := Name;
end;

procedure TMBBook.SetBookISBN( ISBN : String );
begin
     FBookISBN := ISBN;
end;

procedure TMBBook.SetBookYear( Year : String );
begin
     FBookYear := Year;
end;

procedure TMBBook.SetBookNote( Note : String );
begin
     FBookNote := Note;
end;

procedure TMBBook.AddGenre(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='insert into rel_book_genres (book_id, genre_id) values (:bBookID, :bGenreID )';
     SQLQuery.Params.ParamByName('bBookID').AsInteger:=FBookID;
     SQLQuery.Params.ParamByName('bGenreID').AsInteger:=ID;
     SQLQuery.ExecSQL;
     SQLTransaction.Commit;
     FBookGenreID:=ID;
end;

procedure TMBBook.AddPublisher(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='insert into rel_publisher_books (publisher_id, book_id) values (:bPublisherID, :bBookID )';
     SQLQuery.Params.ParamByName('bBookID').AsInteger:=FBookID;
     SQLQuery.Params.ParamByName('bPublisherID').AsInteger:=ID;
     SQLQuery.ExecSQL;
     SQLTransaction.Commit;
     FBookPublisherID:=ID;
end;

procedure TMBBook.AddEditor(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='insert into rel_book_editors (book_id, genre_id) values (:bBookID, :bEditorID )';
     SQLQuery.Params.ParamByName('bBookID').AsInteger:=FBookID;
     SQLQuery.Params.ParamByName('bEditorID').AsInteger:=ID;
     SQLQuery.ExecSQL;
     SQLTransaction.Commit;
     FBookEditorID:=ID;
end;

procedure TMBBook.AddComposition(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='insert into rel_book_compositions (composition_id, book_id) values (:bBookID, :bCompositionID )';
     SQLQuery.Params.ParamByName('bBookID').AsInteger:=FBookID;
     SQLQuery.Params.ParamByName('bCompositionID').AsInteger:=ID;
     SQLQuery.ExecSQL;
     SQLTransaction.Commit;

end;

end.

