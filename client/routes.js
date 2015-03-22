import React from 'react';
import Router from 'react-router';
import { Route, DefaultRoute } from 'react-router';

import App from './app';
import SongContent from './song-content';
import Search from './search';
import Home from './home';

export default (
    <Route name="app" path="/" handler={App}>
        <DefaultRoute handler={Home} />
        <Route name="song" handler={SongContent} />
    </Route>
);
