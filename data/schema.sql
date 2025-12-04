-- Forces table
CREATE TABLE IF NOT EXISTS forces (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    force_id TEXT UNIQUE NOT NULL,
    force_name TEXT NOT NULL
);

-- Crime categories table
schema

CREATE TABLE IF NOT EXISTS crime_categories (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    category_url TEXT UNIQUE NOT NULL,
    category_name TEXT NOT NULL
);

-- Crimes table
CREATE TABLE IF NOT EXISTS crimes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    crime_id TEXT,
    month TEXT NOT NULL,
    latitude REAL NOT NULL,
    longitude REAL NOT NULL,
    street_name TEXT,
    outcome_status TEXT,
    outcome_date TEXT,
    force_ref INTEGER,
    category_ref INTEGER NOT NULL,
    FOREIGN KEY (force_ref) REFERENCES forces(id) ON DELETE SET NULL,
    FOREIGN KEY (category_ref) REFERENCES crime_categories(id) ON DELETE CASCADE
);

-- Indexes for faster searches
CREATE INDEX IF NOT EXISTS idx_crimes_category ON crimes(category_ref);
CREATE INDEX IF NOT EXISTS idx_crimes_force ON crimes(force_ref);
CREATE INDEX IF NOT EXISTS idx_crimes_month ON crimes(month);
CREATE INDEX IF NOT EXISTS idx_crimes_location ON crimes(latitude, longitude);