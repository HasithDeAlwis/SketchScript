export interface File {
    file_id: string; // UUID
    file_project_id: string;
    file_parent_folder_id: string | null;
    file_name: string;
    file_type: string;
    file_content: string | null;
    file_created_at: string; // ISO 8601 string (e.g., from UTCTime)
    file_updated_at: string;
}